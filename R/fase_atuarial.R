#' Executa a Projeção Atuarial do BPC
#'
#' Projeta mensalmente, por espécie, as entradas, as saídas (óbito, revisão e
#' atrito residual) e o estoque ativo de beneficiários.
#'
#' @details
#' \itemize{
#'   \item A tábua biométrica é agregada por idade e espécie, ponderando os
#'     sexos pela população (`n_pessoas_real`).
#'   \item As revisões são alinhadas **pela data** com a projeção. Meses sem
#'     dado em `expectativa_revisao` usam a média da série (com aviso).
#'   \item Se `peso_pcd = NULL` (padrão), as concessões por espécie vêm
#'     diretamente de [projetar_fila_bpc()] (`Con_*_PCD` e `Con_*_IDOSO`). Se for
#'     um número entre 0 e 1, as concessões totais (`Proj_Con_*`) são divididas
#'     com esse peso fixo (comportamento anterior, com 0,578).
#'   \item O estoque ativo parte da âncora (`estoques_ancora`); a distribuição
#'     por idade usada para calcular óbitos vem do último mês do CSV de estoque.
#' }
#'
#' @param df_final_engine Data frame gerado por [projetar_fila_bpc()].
#' @param caminho_tabua Caminho para o arquivo Excel da tábua biométrica.
#' @param caminho_revisoes Caminho para o arquivo Excel de expectativa de
#'   revisões.
#' @param caminho_estoque Caminho para o CSV de estoque agregado.
#' @param estoques_ancora Vetor nomeado ("87" = PCD, "88" = Idoso) com os
#'   estoques iniciais de referência.
#' @param ano_tabua Ano da tábua biométrica a ser utilizado.
#' @param peso_pcd `NULL` ou a proporção de PCD nas concessões totais.
#' @param mes_ref_estoque Mês (inteiro `AAAAMM`, ex.: 202608) do CSV de estoque
#'   usado para a distribuição etária inicial. Se `NULL` (padrão), usa o mês
#'   imediatamente anterior ao primeiro mês projetado; se esse mês não existir
#'   no CSV, usa o mais recente, com aviso.
#' @return Um data frame com o fluxo atuarial mensal por espécie.
#' @export
#' @examples
#' \dontrun{
#'   dt_macro <- obter_dados_macro()
#'   df_fila <- projetar_fila_bpc(dt_macro = dt_macro, horizonte_meses = 12L)
#'   df_atuarial <- executar_motor_atuarial(df_final_engine = df_fila)
#'   # gerar_relatorio_atuarial(...)
#' }
executar_motor_atuarial <- function(
    df_final_engine,
    caminho_tabua = system.file("extdata", "tabua_anualizada.xlsx",
                                package = "mifebpc"),
    caminho_revisoes = system.file("extdata", "expectativa_revisao.xlsx",
                                   package = "mifebpc"),
    caminho_estoque = system.file("extdata",
                                  "bpc_historico_agregado_quadrantes.csv",
                                  package = "mifebpc"),
    estoques_ancora = c("87" = 3781500, "88" = 2740755),
    ano_tabua = 2025L,
    peso_pcd = NULL,
    mes_ref_estoque = NULL
) {
  message("--- [FASE 4] Processamento atuarial ---")

  # --- Validacoes -----------------------------------------------------------
  if (missing(df_final_engine) || !is.data.frame(df_final_engine)) {
    stop("'df_final_engine' deve ser o data frame de projetar_fila_bpc().",
         call. = FALSE)
  }
  .checar_arquivo(caminho_tabua, "tabua biometrica (tabua_anualizada.xlsx)")
  .checar_arquivo(caminho_revisoes, "expectativa de revisoes (expectativa_revisao.xlsx)")
  .checar_arquivo(caminho_estoque, "estoque agregado (bpc_historico_agregado_quadrantes.csv)")

  if (!all(c("87", "88") %in% names(estoques_ancora))) {
    stop("'estoques_ancora' deve ser um vetor nomeado com '87' e '88'.",
         call. = FALSE)
  }

  usar_peso_fixo <- !is.null(peso_pcd)
  if (usar_peso_fixo) {
    if (!is.numeric(peso_pcd) || length(peso_pcd) != 1L || is.na(peso_pcd) ||
        peso_pcd <= 0 || peso_pcd >= 1) {
      stop("'peso_pcd' deve ser NULL ou um numero entre 0 e 1.", call. = FALSE)
    }
    .checar_colunas(df_final_engine, c("Proj_Con_Adm", "Proj_Con_Jud"),
                    "df_final_engine")
  } else {
    .checar_colunas(
      df_final_engine,
      c("Con_Adm_PCD", "Con_Adm_IDOSO", "Con_Jud_PCD", "Con_Jud_IDOSO"),
      "df_final_engine"
    )
  }
  .checar_colunas(
    df_final_engine,
    c("Data", "TMC_Adm_PCD", "TMC_Adm_IDOSO", "TMC_Jud_PCD", "TMC_Jud_IDOSO"),
    "df_final_engine"
  )

  num_linhas <- nrow(df_final_engine)
  datas_proj <- .inicio_mes(df_final_engine$Data)

  # --- Tabua: agrega os sexos por idade e especie ---------------------------
  tabua_raw <- readxl::read_excel(caminho_tabua)
  names(tabua_raw) <- trimws(names(tabua_raw))
  .checar_colunas(tabua_raw,
                  c("Ano", "CS_ESPECIE", "idade", "q_x", "n_pessoas_real"),
                  "tabua biometrica")

  tabua_full <- tabua_raw %>%
    dplyr::filter(Ano == ano_tabua) %>%
    dplyr::group_by(CS_ESPECIE, idade) %>%
    dplyr::summarise(
      q_x = if (sum(n_pessoas_real, na.rm = TRUE) > 0) {
        stats::weighted.mean(q_x, n_pessoas_real, na.rm = TRUE)
      } else {
        mean(q_x, na.rm = TRUE)
      },
      .groups = "drop"
    ) %>%
    dplyr::mutate(qx_mensal = 1 - (1 - q_x)^(1 / 12))

  if (nrow(tabua_full) == 0L) {
    stop("A tabua nao tem registros para o ano ", ano_tabua, ".", call. = FALSE)
  }

  # --- Estoque por idade (ultimo mes do CSV) --------------------------------
  # fill = TRUE: os ultimos meses do CSV nao trazem as duas ultimas colunas
  # (cessacoes_adm e saidas_residuais). Sem isso, o fread descarta o arquivo
  # a partir da primeira linha curta.
  df_limpo <- as.data.frame(data.table::fread(caminho_estoque, fill = TRUE))
  names(df_limpo) <- tolower(names(df_limpo))
  .checar_colunas(
    df_limpo,
    c("nu_mes_ref", "cs_especie", "idade", "estoque_ativo", "saidas_residuais"),
    "estoque agregado"
  )
  df_limpo$cs_especie <- as.character(df_limpo$cs_especie)
  df_limpo$nu_mes_ref <- as.integer(df_limpo$nu_mes_ref)

  # Mes de referencia da distribuicao etaria: o mes anterior ao inicio da
  # projecao (a ancora de estoque corresponde ao fim desse mes).
  meses_disponiveis <- sort(unique(df_limpo$nu_mes_ref))
  if (is.null(mes_ref_estoque)) {
    mes_anterior <- seq.Date(min(datas_proj), by = "-1 month",
                             length.out = 2L)[2L]
    mes_ref_estoque <- as.integer(format(mes_anterior, "%Y%m"))
  }
  if (!(mes_ref_estoque %in% meses_disponiveis)) {
    mes_ref_estoque <- max(meses_disponiveis, na.rm = TRUE)
    warning("Mes de referencia do estoque nao encontrado no CSV; usando o mais ",
            "recente (", mes_ref_estoque, ").", call. = FALSE)
  }
  mes_max <- mes_ref_estoque
  message("Distribuicao etaria do estoque: mes ", mes_max)

  # Vetor de 101 posicoes: posicao k = idade k - 1 (0 a 100 anos).
  # Ignora idades negativas (erro de cadastro) e acima de 100.
  distribuicao_estoque <- function(cod_especie) {
    d <- df_limpo[
      df_limpo$nu_mes_ref == mes_max &
        df_limpo$cs_especie == cod_especie &
        !is.na(df_limpo$idade) &
        df_limpo$idade >= 0 & df_limpo$idade <= 100,
      c("idade", "estoque_ativo")
    ]
    v <- numeric(101)
    if (nrow(d) > 0L) {
      agg <- tapply(d$estoque_ativo, d$idade, sum, na.rm = TRUE)
      v[as.integer(names(agg)) + 1L] <- as.numeric(agg)
    }
    v
  }

  vetor_pcd <- distribuicao_estoque("87")
  vetor_ido <- distribuicao_estoque("88")

  if (sum(vetor_pcd) <= 0 || sum(vetor_ido) <= 0) {
    stop("Estoque do ultimo mes (", mes_max,
         ") esta vazio para alguma especie.", call. = FALSE)
  }

  # Conferencia: o total do mes de referencia deve ficar proximo da ancora.
  # Um desvio grande costuma indicar mes duplicado ou incompleto no CSV.
  for (esp_chk in c("87", "88")) {
    total_ref <- sum(if (esp_chk == "87") vetor_pcd else vetor_ido)
    ancora <- unname(estoques_ancora[[esp_chk]])
    if (abs(total_ref / ancora - 1) > 0.10) {
      warning(
        "Estoque do mes ", mes_max, " (especie ", esp_chk, ") = ",
        format(round(total_ref), big.mark = "."), ", mais de 10% distante da ",
        "ancora (", format(ancora, big.mark = "."), "). Verifique o CSV.",
        call. = FALSE
      )
    }
  }

  # Novas entradas PCD seguem a distribuicao etaria do estoque atual.
  dist_idade_pcd <- vetor_pcd / sum(vetor_pcd)

  # --- Revisoes alinhadas por data ------------------------------------------
  df_rev_raw <- readxl::read_excel(caminho_revisoes)
  names(df_rev_raw) <- trimws(names(df_rev_raw))
  .checar_colunas(
    df_rev_raw,
    c("nu_mes_ref", "cessacao_pcd_feminino", "cessacao_pcd_masculino",
      "cessacao_idoso_feminino", "cessacao_idoso_masculino"),
    "expectativa de revisoes"
  )

  datas_rev <- .inicio_mes(df_rev_raw$nu_mes_ref)
  pos_rev <- match(datas_proj, datas_rev)
  n_sem_rev <- sum(is.na(pos_rev))
  if (n_sem_rev > 0L) {
    warning(
      n_sem_rev, " mes(es) da projecao nao tem dado em 'expectativa_revisao'; ",
      "usando a media da serie disponivel nesses meses.",
      call. = FALSE
    )
  }

  alinhar_revisoes <- function(valores) {
    valores <- as.numeric(valores)
    out <- valores[pos_rev]
    out[is.na(out)] <- mean(valores, na.rm = TRUE)
    out
  }

  revs_pcd <- alinhar_revisoes(df_rev_raw$cessacao_pcd_feminino +
                                 df_rev_raw$cessacao_pcd_masculino)
  revs_ido <- alinhar_revisoes(df_rev_raw$cessacao_idoso_feminino +
                                 df_rev_raw$cessacao_idoso_masculino)

  # --- Loop por especie -----------------------------------------------------
  lista_fluxos <- list()

  for (esp in c("87", "88")) {
    nome_esp <- if (esp == "87") "PCD" else "IDOSO"
    estoque_inicial <- unname(estoques_ancora[[esp]])

    # Mortalidade mensal por idade
    tabua_esp <- tabua_full[tabua_full$CS_ESPECIE == nome_esp, ]
    if (nrow(tabua_esp) == 0L) {
      stop("A tabua nao tem registros para a especie ", nome_esp, ".",
           call. = FALSE)
    }
    qx_vetor <- numeric(101)
    ids <- tabua_esp$idade + 1
    ok <- ids >= 1 & ids <= 101
    qx_vetor[ids[ok]] <- tabua_esp$qx_mensal[ok]

    # Concessoes
    if (usar_peso_fixo) {
      peso <- if (esp == "87") peso_pcd else 1 - peso_pcd
      entrada_adm <- as.numeric(df_final_engine$Proj_Con_Adm) * peso
      entrada_jud <- as.numeric(df_final_engine$Proj_Con_Jud) * peso
    } else if (esp == "87") {
      entrada_adm <- as.numeric(df_final_engine$Con_Adm_PCD)
      entrada_jud <- as.numeric(df_final_engine$Con_Jud_PCD)
    } else {
      entrada_adm <- as.numeric(df_final_engine$Con_Adm_IDOSO)
      entrada_jud <- as.numeric(df_final_engine$Con_Jud_IDOSO)
    }
    concessao_total <- entrada_adm + entrada_jud

    # Atrito residual medio mensal
    # Apenas linhas completas: nos meses sem 'saidas_residuais' o estoque entraria
    # no denominador sem o numerador, subestimando a taxa.
    d_esp <- df_limpo[
      df_limpo$cs_especie == esp &
        !is.na(df_limpo$saidas_residuais) &
        !is.na(df_limpo$estoque_ativo),
    ]
    taxa_atrito <- sum(d_esp$saidas_residuais, na.rm = TRUE) /
      sum(d_esp$estoque_ativo, na.rm = TRUE)
    if (!is.finite(taxa_atrito) || taxa_atrito == 0) taxa_atrito <- 0.0021

    tmc_adm <- if (esp == "87") df_final_engine$TMC_Adm_PCD else df_final_engine$TMC_Adm_IDOSO
    tmc_jud <- if (esp == "87") df_final_engine$TMC_Jud_PCD else df_final_engine$TMC_Jud_IDOSO

    saida_obito <- numeric(num_linhas)
    saida_residual <- numeric(num_linhas)

    estoque_temp <- if (esp == "87") vetor_pcd else vetor_ido

    for (i in seq_len(num_linhas)) {
      estoque_temp[is.na(estoque_temp)] <- 0

      obitos_rodada <- estoque_temp * qx_vetor
      atrito_rodada <- estoque_temp * taxa_atrito

      saida_obito[i] <- sum(obitos_rodada, na.rm = TRUE)
      saida_residual[i] <- sum(atrito_rodada, na.rm = TRUE)

      estoque_vivos <- estoque_temp - obitos_rodada - atrito_rodada
      estoque_vivos[estoque_vivos < 0] <- 0

      # Envelhecimento anual (a cada 12 meses): todos avancam um ano de idade
      if (i %% 12 == 0) {
        estoque_vivos <- c(0, estoque_vivos[1:100])
      }

      if (esp == "88") {
        # Idosos entram aos 65 anos (posicao 66)
        estoque_vivos[66] <- estoque_vivos[66] + concessao_total[i]
        estoque_temp <- estoque_vivos
      } else {
        estoque_temp <- estoque_vivos + concessao_total[i] * dist_idade_pcd
      }
    }

    saida_revisao <- if (esp == "87") revs_pcd else revs_ido

    lista_fluxos[[esp]] <- data.frame(
      Data = df_final_engine$Data,
      especie = nome_esp,
      Entrada_Adm = entrada_adm,
      Entrada_Jud = entrada_jud,
      TMC_Adm_Meses = tmc_adm / 30,
      TMC_Jud_Meses = tmc_jud / 30,
      Saida_Obito = saida_obito,
      Saida_Revisao = saida_revisao,
      Saida_Residual = saida_residual,
      Estoque_Ativo = estoque_inicial +
        cumsum(concessao_total - saida_obito - saida_residual - saida_revisao),
      stringsAsFactors = FALSE
    )
  }

  dplyr::bind_rows(lista_fluxos)
}
