#' Projeta a Fila do BPC
#'
#' Projeta, por espécie (PCD e Idoso), as concessões administrativas, as
#' concessões judiciais e o tempo médio de concessão (TMC).
#'
#' @details
#' \itemize{
#'   \item **Concessões** (`Deferidos_*` e `judicial_*`): random forest treinado
#'     com as variáveis macroeconômicas (`ipca`, `taxa_desemprego`, `pop_idosa`,
#'     `taxa_informalidade`).
#'   \item **TMC**: média dos últimos 12 meses com oscilação senoidal e ruído
#'     aleatório proporcionais ao desvio-padrão (não é um modelo estatístico) e
#'     truncado em zero.
#'   \item As variáveis macro futuras são mantidas no último valor observado
#'     (`pop_idosa` e `taxa_informalidade` crescem a passos fixos). Como o
#'     random forest não extrapola, a projeção tende a ser quase constante.
#'   \item A semente é aplicada localmente e não altera o gerador de números
#'     aleatórios da sessão do usuário.
#' }
#'
#' @param caminho_fila Caminho para o arquivo Excel do histórico de fila
#'   processado.
#' @param dt_macro Data frame com os dados macroeconômicos, como o retornado por
#'   [obter_dados_macro()].
#' @param horizonte_meses Número de meses a projetar (padrão 60).
#' @param data_inicio_projecao Data de início da projeção (opcional).
#' @param semente Semente para reprodutibilidade.
#' @return Um tibble com `Data`, as concessões por espécie (`Con_Adm_PCD`,
#'   `Con_Adm_IDOSO`, `Con_Jud_PCD`, `Con_Jud_IDOSO`), os totais (`Proj_Con_Adm`,
#'   `Proj_Con_Jud`) e os TMC em dias (`TMC_Adm_PCD`, `TMC_Adm_IDOSO`,
#'   `TMC_Jud_PCD`, `TMC_Jud_IDOSO`).
#' @examples
#' \donttest{
#' dt_macro <- obter_dados_macro()
#' df_fila <- projetar_fila_bpc(dt_macro = dt_macro)
#' }
#' @export
projetar_fila_bpc <- function(
    caminho_fila = system.file("extdata", "bpc_historico_fila_processado.xlsx",
                               package = "mifebpc"),
    dt_macro,
    horizonte_meses = 60L, # O utilizador pode alterar para 12L, 24L, etc.
    data_inicio_projecao = NULL,
    semente = 42L
) {
  message("--- [FASE 1-3] Modelando e projetando a fila do BPC ---")

  if (missing(dt_macro) || is.null(dt_macro) || !is.data.frame(dt_macro)) {
    stop("O objeto dt_macro nao foi fornecido ou e invalido.", call. = FALSE)
  }
  .checar_arquivo(caminho_fila,
                  "historico da fila (bpc_historico_fila_processado.xlsx)")

  horizonte_meses <- as.integer(horizonte_meses)
  if (length(horizonte_meses) != 1L || is.na(horizonte_meses) ||
      horizonte_meses < 1L) {
    stop("'horizonte_meses' deve ser um inteiro positivo.", call. = FALSE)
  }

  vars_macro <- c("ipca", "taxa_desemprego", "pop_idosa",
                  "taxa_informalidade")
  .checar_colunas(dt_macro, c("Data", vars_macro), "dt_macro")

  # --- Histórico da fila ---
  dt_fila <- readxl::read_excel(caminho_fila, sheet = 1)
  names(dt_fila) <- trimws(names(dt_fila))
  .checar_colunas(dt_fila, "mes_index", "historico da fila")
  dt_fila$data_mife <- .inicio_mes(dt_fila$mes_index)

  # SE O UTILIZADOR INFORMOU UMA DATA DE CORTE (ex: "2027-01-01")
  if (!is.null(data_inicio_projecao)) {
    data_corte <- as.Date(data_inicio_projecao) - months(1) # O último mês de histórico real permitido
    dt_fila <- dt_fila %>% dplyr::filter(data_mife <= data_corte)
  }

  # Restante da rotina continua igual, detetando automaticamente a última data disponível
  df_macro_hist <- dt_macro %>%
    tibble::as_tibble() %>%
    dplyr::mutate(data_mife = .inicio_mes(Data)) %>%
    dplyr::distinct(data_mife, .keep_all = TRUE) %>%
    dplyr::select(data_mife, dplyr::all_of(vars_macro))

  df_modelagem <- dt_fila %>%
    dplyr::left_join(df_macro_hist, by = "data_mife") %>%
    dplyr::arrange(data_mife) %>%
    dplyr::mutate(dplyr::across(
      dplyr::all_of(vars_macro),
      ~ zoo::na.locf(.x, na.rm = FALSE)
    )) %>%
    dplyr::filter(!is.na(ipca))

  ultima_data <- max(df_modelagem$data_mife)
  message("Data maxima de treino/historico considerada: ", format(ultima_data))

  datas_futuras <- seq.Date(ultima_data, by = "month",
                            length.out = horizonte_meses + 1L)[-1L]

  if (nrow(df_modelagem) < 12L) {
    stop("Historico insuficiente apos cruzar fila e dados macro (",
         nrow(df_modelagem), " meses; minimo de 12).", call. = FALSE)
  }

  nomes_saida <- c(
    "Deferidos_BPC_PCD"     = "Deferidos_BPC_PCD",
    "Deferidos_BPC_IDOSO"   = "Deferidos_BPC_IDOSO",
    "judicial_PCD"          = "judicial_PCD",
    "judicial_IDOSO"        = "judicial_IDOSO",
    "TMC_BPC PCD_dias"      = "TMC_Adm_PCD",
    "TMC_BPC IDOSO_dias"    = "TMC_Adm_IDOSO",
    "TMC_judicialPCD"       = "TMC_Jud_PCD",
    "TMC_judicialIDOSO"     = "TMC_Jud_IDOSO"
  )
  .checar_colunas(df_modelagem, names(nomes_saida), "historico da fila")

  # --- Macro futura com base no horizonte informado ---
  ultima_data <- max(df_modelagem$data_mife)
  message("Data maxima detectada: ", format(ultima_data))

  datas_futuras <- seq.Date(ultima_data, by = "month",
                            length.out = horizonte_meses + 1L)[-1L]

  ultimo <- vapply(
    vars_macro,
    function(v) utils::tail(stats::na.omit(df_modelagem[[v]]), 1L),
    numeric(1)
  )

  df_macro_futuro <- tibble::tibble(
    Data               = datas_futuras,
    ipca               = ultimo[["ipca"]],
    taxa_desemprego    = ultimo[["taxa_desemprego"]],
    pop_idosa          = seq(ultimo[["pop_idosa"]], by = 5000,
                             length.out = horizonte_meses),
    taxa_informalidade = seq(ultimo[["taxa_informalidade"]], by = 0.01,
                             length.out = horizonte_meses)
  )

  # --- Projeção com base no horizonte dinâmico ---
  lista_projecoes <- withr::with_seed(semente, {
    saida <- list()

    for (m in names(nomes_saida)) {
      nome_chave <- nomes_saida[[m]]

      if (grepl("^TMC", m)) {
        historico <- utils::tail(stats::na.omit(df_modelagem[[m]]), 12L)
        media_base <- mean(historico)
        desvio <- stats::sd(historico)
        if (!is.finite(desvio) || desvio == 0) desvio <- 1.5

        proj <- media_base +
          sin(seq_len(horizonte_meses)) * (desvio * 0.8) +
          stats::rnorm(horizonte_meses, mean = 0, sd = desvio * 0.2)

        saida[[nome_chave]] <- pmax(proj, 0)
      } else {
        df_treino <- stats::na.omit(
          as.data.frame(df_modelagem[, c(m, vars_macro)])
        )
        if (nrow(df_treino) < 12L) {
          stop("Poucas observacoes completas para modelar '", m, "'.",
               call. = FALSE)
        }

        modelo <- randomForest::randomForest(
          x = df_treino[, vars_macro, drop = FALSE],
          y = as.numeric(df_treino[[m]]),
          ntree = 500
        )

        x_futuro <- as.data.frame(df_macro_futuro[, vars_macro, drop = FALSE])
        saida[[nome_chave]] <- as.numeric(stats::predict(modelo,
                                                         newdata = x_futuro))
      }
    }
    saida
  })

  p <- lista_projecoes

  tibble::tibble(
    Data          = datas_futuras,
    Con_Adm_PCD   = p$Deferidos_BPC_PCD,
    Con_Adm_IDOSO = p$Deferidos_BPC_IDOSO,
    Con_Jud_PCD   = p$judicial_PCD,
    Con_Jud_IDOSO = p$judicial_IDOSO,
    Proj_Con_Adm  = p$Deferidos_BPC_PCD + p$Deferidos_BPC_IDOSO,
    Proj_Con_Jud  = p$judicial_PCD + p$judicial_IDOSO,
    TMC_Adm_PCD   = p$TMC_Adm_PCD,
    TMC_Adm_IDOSO = p$TMC_Adm_IDOSO,
    TMC_Jud_PCD   = p$TMC_Jud_PCD,
    TMC_Jud_IDOSO = p$TMC_Jud_IDOSO
  )
}
