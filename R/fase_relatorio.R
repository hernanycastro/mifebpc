#' Gera o Relatório Atuarial (físico e financeiro)
#'
#' Reproduz, a partir do fluxo atuarial mensal, as abas do relatório em Excel
#' `Estoque_Mensal_Detalhado`, `Planilha1`, `resumo_detalhes` e `resumo`.
#' A aba `resumo_financeiro` (que depende de valores executados, não
#' disponíveis no pacote) não está incluída.
#'
#' @details
#' Fórmulas (todas por espécie PCD/Idoso; BPC = PCD + Idoso):
#' \itemize{
#'   \item **Retroativo** (adm e jud) = entradas × TMC (em meses) × salário
#'     mínimo (SM).
#'   \item **Juros e correção** = retroativo × `taxa_juros`.
#'   \item **Total** = estoque × SM + retroativo adm + retroativo jud + juros
#'     adm + juros jud.
#'   \item **IC inferior / superior** = Total BPC × (1 -/+ `ic`).
#' }
#'
#' Na tabela mensal (`planilha1`) os cálculos são feitos mês a mês. Na tabela
#' anual (`resumo_detalhes`) o retroativo usa a **soma das entradas do ano ×
#' média anual da TMC** (como na aba `resumo_detalhes` do Excel), e o custo do
#' estoque usa a soma dos estoques mensais do ano × SM. Por isso o total anual
#' não é exatamente a soma dos meses. O estoque anual é o do último mês do ano
#' presente nos dados.
#'
#' Pontos de atenção:
#' \itemize{
#'   \item A coluna `Meses` indica quantos meses do ano estão na projeção. Anos
#'     parciais (primeiro e último) não são comparáveis aos anos completos, e o
#'     estoque "dez" desses anos é o do último mês disponível.
#'   \item Anos sem salário mínimo em `tabela_sm` têm as colunas financeiras
#'     `NA` (com aviso). Informe `tabela_sm` com todos os anos da projeção.
#'   \item `Saidas_obito_BPC` soma óbitos de PCD e Idoso. (Na aba `resumo` do
#'     Excel, a fórmula de óbitos somava apenas PCD, o que subestimava as saídas
#'     e a razão S/E.)
#' }
#'
#' @param df_fluxo_detalhado Data frame gerado por [executar_motor_atuarial()].
#' @param taxa_juros Taxa de juros e correção aplicada sobre o retroativo.
#' @param tabela_sm Data frame com as colunas `Ano` e `SM` (salário mínimo).
#' @param ic Amplitude do intervalo de confiança (0,05 = ±5%).
#' @param caminho_xlsx Caminho de um arquivo `.xlsx` para gravar o relatório
#'   (abas `resumo`, `resumo_detalhes`, `Planilha1` e `Estoque_Mensal_Detalhado`).
#'   Se `NULL` (padrão), nada é gravado.
#' @return Uma lista com cinco tabelas (tibbles): `estoque_mensal_detalhado`,
#'   `planilha1`, `resumo_detalhes`, `resumo_fisico_financeiro` e
#'   `resumo_entradas_saidas` (os dois blocos da aba `resumo`).
#' @examples
#' \donttest{
#' dt_macro <- obter_dados_macro()
#' df_fila <- projetar_fila_bpc(dt_macro = dt_macro)
#' df_atuarial <- executar_motor_atuarial(df_fila)
#' rel <- gerar_relatorio_atuarial(
#'   df_atuarial,
#'   caminho_xlsx = tempfile(fileext = ".xlsx")
#' )
#' rel$resumo_fisico_financeiro
#' }
#' @export
gerar_relatorio_atuarial <- function(
    df_fluxo_detalhado,
    taxa_juros = 0.0403,
    tabela_sm = tibble::tibble(
      Ano = 2026:2031,
      SM = c(1621, 1741, 1854, 1956, 2066, 2180)
    ),
    ic = 0.05,
    caminho_xlsx = NULL
) {
  message("--- [FASE 5] Gerando relatorio atuarial ---")

  # --- Validacoes -----------------------------------------------------------
  if (missing(df_fluxo_detalhado) || !is.data.frame(df_fluxo_detalhado)) {
    stop("'df_fluxo_detalhado' deve ser o resultado de executar_motor_atuarial().",
         call. = FALSE)
  }
  if (!is.numeric(taxa_juros) || length(taxa_juros) != 1L ||
      is.na(taxa_juros) || taxa_juros < 0) {
    stop("'taxa_juros' deve ser um numero nao negativo.", call. = FALSE)
  }
  if (!is.numeric(ic) || length(ic) != 1L || is.na(ic) || ic < 0 || ic >= 1) {
    stop("'ic' deve ser um numero em [0, 1).", call. = FALSE)
  }

  variaveis <- c("Estoque_Ativo", "Entrada_Adm", "Entrada_Jud",
                 "TMC_Adm_Meses", "TMC_Jud_Meses", "Saida_Obito",
                 "Saida_Revisao", "Saida_Residual")
  .checar_colunas(df_fluxo_detalhado, c("Data", "especie", variaveis),
                  "df_fluxo_detalhado")
  .checar_colunas(tabela_sm, c("Ano", "SM"), "tabela_sm")

  if (!all(c("PCD", "IDOSO") %in% df_fluxo_detalhado$especie)) {
    stop("'especie' deve conter os valores 'PCD' e 'IDOSO'.", call. = FALSE)
  }
  df_fluxo_detalhado$Data <- as.Date(df_fluxo_detalhado$Data)
  if (anyDuplicated(as.data.frame(df_fluxo_detalhado[, c("Data", "especie")])) > 0L) {
    stop("Ha mais de uma linha por Data e especie em 'df_fluxo_detalhado'.",
         call. = FALSE)
  }

  # --- 1. Estoque_Mensal_Detalhado (formato largo) ---------------------------
  cols_ordem <- c(
    "Data",
    as.vector(rbind(paste0(variaveis, "_PCD"), paste0(variaveis, "_IDOSO")))
  )

  largo <- tidyr::pivot_wider(
    tibble::as_tibble(df_fluxo_detalhado[, c("Data", "especie", variaveis)]),
    id_cols = "Data",
    names_from = "especie",
    values_from = dplyr::all_of(variaveis)
  )
  e <- as.data.frame(largo[order(largo$Data), cols_ordem])

  e$Total_fisico_PCD   <- e$Estoque_Ativo_PCD
  e$Total_fisico_Idoso <- e$Estoque_Ativo_IDOSO
  e$Total_fisico_BPC   <- e$Estoque_Ativo_PCD + e$Estoque_Ativo_IDOSO

  # --- Salario minimo por mes -------------------------------------------------
  anos <- as.integer(format(e$Data, "%Y"))
  sm <- as.numeric(tabela_sm$SM[match(anos, tabela_sm$Ano)])
  anos_sem_sm <- sort(unique(anos[is.na(sm)]))
  if (length(anos_sem_sm) > 0L) {
    warning(
      "Sem salario minimo em 'tabela_sm' para o(s) ano(s): ",
      paste(anos_sem_sm, collapse = ", "),
      ". As colunas financeiras desses anos ficam NA.",
      call. = FALSE
    )
  }

  # --- 2. Planilha1 (mensal) --------------------------------------------------
  fin_m <- .calcular_financeiro(
    adm_pcd = e$Entrada_Adm_PCD, adm_ido = e$Entrada_Adm_IDOSO,
    jud_pcd = e$Entrada_Jud_PCD, jud_ido = e$Entrada_Jud_IDOSO,
    tmc_adm_pcd = e$TMC_Adm_Meses_PCD, tmc_adm_ido = e$TMC_Adm_Meses_IDOSO,
    tmc_jud_pcd = e$TMC_Jud_Meses_PCD, tmc_jud_ido = e$TMC_Jud_Meses_IDOSO,
    fis_pcd = e$Total_fisico_PCD, fis_ido = e$Total_fisico_Idoso,
    sm = sm, taxa_juros = taxa_juros, ic = ic
  )

  fisico_m <- data.frame(
    Data = e$Data,
    Estoque_PCD = e$Estoque_Ativo_PCD,
    Estoque_idoso = e$Estoque_Ativo_IDOSO,
    Estoque_BPC = e$Total_fisico_BPC,
    Entradas_adm_BPC = e$Entrada_Adm_PCD + e$Entrada_Adm_IDOSO,
    Entradas_jud_BPC = e$Entrada_Jud_PCD + e$Entrada_Jud_IDOSO,
    Saidas_obito_BPC = e$Saida_Obito_PCD + e$Saida_Obito_IDOSO,
    Saida_Revisao_BPC = e$Saida_Revisao_PCD + e$Saida_Revisao_IDOSO,
    Saidas_outras_BPC = e$Saida_Residual_PCD + e$Saida_Residual_IDOSO
  )
  superior_m <- data.frame(
    Total_PCD_superior = fin_m$Total_PCD * (1 + ic),
    Total_Idoso_superior = fin_m$Total_Idoso * (1 + ic)
  )
  planilha1 <- tibble::as_tibble(cbind(fisico_m, fin_m, superior_m))

  # --- 3. resumo_detalhes (anual) --------------------------------------------
  anos_u <- sort(unique(anos))
  agregar <- function(coluna, f) {
    vapply(anos_u, function(a) f(e[[coluna]][anos == a]), numeric(1))
  }
  soma   <- function(coluna) agregar(coluna, sum)
  media  <- function(coluna) agregar(coluna, mean)
  ultimo <- function(coluna) agregar(coluna, function(x) x[length(x)])
  sm_a   <- vapply(anos_u, function(a) sm[anos == a][1L], numeric(1))

  fin_a <- .calcular_financeiro(
    adm_pcd = soma("Entrada_Adm_PCD"), adm_ido = soma("Entrada_Adm_IDOSO"),
    jud_pcd = soma("Entrada_Jud_PCD"), jud_ido = soma("Entrada_Jud_IDOSO"),
    tmc_adm_pcd = media("TMC_Adm_Meses_PCD"),
    tmc_adm_ido = media("TMC_Adm_Meses_IDOSO"),
    tmc_jud_pcd = media("TMC_Jud_Meses_PCD"),
    tmc_jud_ido = media("TMC_Jud_Meses_IDOSO"),
    fis_pcd = soma("Total_fisico_PCD"), fis_ido = soma("Total_fisico_Idoso"),
    sm = sm_a, taxa_juros = taxa_juros, ic = ic
  )

  fisico_a <- data.frame(
    Ano = anos_u,
    Estoque_PCD = ultimo("Estoque_Ativo_PCD"),
    Estoque_idoso = ultimo("Estoque_Ativo_IDOSO"),
    Estoque_BPC = ultimo("Estoque_Ativo_PCD") + ultimo("Estoque_Ativo_IDOSO"),
    Entradas_adm_BPC = soma("Entrada_Adm_PCD") + soma("Entrada_Adm_IDOSO"),
    Entradas_jud_BPC = soma("Entrada_Jud_PCD") + soma("Entrada_Jud_IDOSO"),
    Saidas_obito_BPC = soma("Saida_Obito_PCD") + soma("Saida_Obito_IDOSO"),
    Saida_Revisao_BPC = soma("Saida_Revisao_PCD") + soma("Saida_Revisao_IDOSO"),
    Saidas_outras_BPC = soma("Saida_Residual_PCD") + soma("Saida_Residual_IDOSO")
  )
  meses_a <- data.frame(
    Meses = as.integer(vapply(anos_u, function(a) sum(anos == a), numeric(1)))
  )
  resumo_detalhes <- tibble::as_tibble(cbind(fisico_a, fin_a, meses_a))

  # --- 4. resumo (dois blocos) ------------------------------------------------
  resumo_fisico_financeiro <- tibble::tibble(
    Ano = resumo_detalhes$Ano,
    Estoque_PCD = resumo_detalhes$Estoque_PCD,
    Estoque_idoso = resumo_detalhes$Estoque_idoso,
    Estoque_BPC = resumo_detalhes$Estoque_BPC,
    Total_PCD = resumo_detalhes$Total_PCD,
    Total_Idoso = resumo_detalhes$Total_Idoso,
    Total_BPC = resumo_detalhes$Total_BPC,
    IC_inferior_BPC = resumo_detalhes$IC_inferior,
    IC_Superior_BPC = resumo_detalhes$IC_Superior
  )

  resumo_entradas <- resumo_detalhes$Entradas_adm_BPC +
    resumo_detalhes$Entradas_jud_BPC
  resumo_saidas <- resumo_detalhes$Saidas_obito_BPC +
    resumo_detalhes$Saida_Revisao_BPC + resumo_detalhes$Saidas_outras_BPC
  razao <- resumo_saidas / resumo_entradas
  razao[!is.finite(razao)] <- 0   # equivalente ao IFERROR(..., 0) do Excel

  resumo_entradas_saidas <- tibble::tibble(
    Ano = resumo_detalhes$Ano,
    Entradas_adm_BPC = resumo_detalhes$Entradas_adm_BPC,
    Entradas_jud_BPC = resumo_detalhes$Entradas_jud_BPC,
    Saidas_obito_BPC = resumo_detalhes$Saidas_obito_BPC,
    Saida_Revisao_BPC = resumo_detalhes$Saida_Revisao_BPC,
    Saidas_outras_BPC = resumo_detalhes$Saidas_outras_BPC,
    Resumo_Entradas = resumo_entradas,
    Resumo_Saidas = resumo_saidas,
    Razao_S_E = razao
  )

  relatorio <- list(
    estoque_mensal_detalhado = tibble::as_tibble(e),
    planilha1 = planilha1,
    resumo_detalhes = resumo_detalhes,
    resumo_fisico_financeiro = resumo_fisico_financeiro,
    resumo_entradas_saidas = resumo_entradas_saidas
  )

  if (!is.null(caminho_xlsx)) {
    .exportar_relatorio_xlsx(relatorio, caminho_xlsx)
    message("Relatorio gravado em: ", caminho_xlsx)
  }

  relatorio
}

# ---------------------------------------------------------------------------
# Funcoes internas
# ---------------------------------------------------------------------------

# Bloco financeiro. Recebe entradas, TMC (meses) e total fisico ja agregados
# (por mes, ou por ano) e devolve as colunas de retroativo, juros, total e IC.
.calcular_financeiro <- function(adm_pcd, adm_ido, jud_pcd, jud_ido,
                                 tmc_adm_pcd, tmc_adm_ido,
                                 tmc_jud_pcd, tmc_jud_ido,
                                 fis_pcd, fis_ido, sm, taxa_juros, ic) {
  ret_adm_pcd <- adm_pcd * tmc_adm_pcd * sm
  ret_adm_ido <- adm_ido * tmc_adm_ido * sm
  ret_jud_pcd <- jud_pcd * tmc_jud_pcd * sm
  ret_jud_ido <- jud_ido * tmc_jud_ido * sm
  ret_adm_bpc <- ret_adm_pcd + ret_adm_ido
  ret_jud_bpc <- ret_jud_pcd + ret_jud_ido

  jur_adm_pcd <- ret_adm_pcd * taxa_juros
  jur_adm_ido <- ret_adm_ido * taxa_juros
  jur_jud_pcd <- ret_jud_pcd * taxa_juros
  jur_jud_ido <- ret_jud_ido * taxa_juros
  jur_adm_bpc <- ret_adm_bpc * taxa_juros
  jur_jud_bpc <- ret_jud_bpc * taxa_juros

  total_pcd <- fis_pcd * sm +
    (ret_adm_pcd + ret_jud_pcd + jur_adm_pcd + jur_jud_pcd)
  total_ido <- fis_ido * sm +
    (ret_adm_ido + ret_jud_ido + jur_adm_ido + jur_jud_ido)
  total_bpc <- (fis_pcd + fis_ido) * sm +
    (ret_adm_bpc + ret_jud_bpc + jur_adm_bpc + jur_jud_bpc)

  data.frame(
    Retroativo_adm_PCD = ret_adm_pcd,
    Retroativo_adm_Idoso = ret_adm_ido,
    Retroativo_adm_BPC = ret_adm_bpc,
    Retroativo_jud_PCD = ret_jud_pcd,
    Retroativo_jud_Idoso = ret_jud_ido,
    Retroativo_jud_BPC = ret_jud_bpc,
    Juros_Correcao_adm_PCD = jur_adm_pcd,
    Juros_Correcao_adm_Idoso = jur_adm_ido,
    Juros_Correcao_adm_BPC = jur_adm_bpc,
    Juros_Correcao_jud_PCD = jur_jud_pcd,
    Juros_Correcao_jud_Idoso = jur_jud_ido,
    Juros_Correcao_jud_BPC = jur_jud_bpc,
    Total_PCD = total_pcd,
    Total_Idoso = total_ido,
    Total_BPC = total_bpc,
    IC_inferior = total_bpc * (1 - ic),
    IC_Superior = total_bpc * (1 + ic)
  )
}

# Acrescenta " R$" ao nome das colunas monetarias (cabecalhos do Excel).
.rotular_reais <- function(df) {
  df <- as.data.frame(df)
  idx <- grepl("^(Retroativo_|Juros_Correcao_|Total_PCD|Total_Idoso|Total_BPC)",
               names(df))
  names(df)[idx] <- paste0(names(df)[idx], " R$")
  df
}

# Grava o relatorio em .xlsx com as mesmas abas e cabecalhos do original.
.exportar_relatorio_xlsx <- function(relatorio, caminho) {
  if (!is.character(caminho) || length(caminho) != 1L || is.na(caminho) ||
      !grepl("\\.xlsx$", caminho, ignore.case = TRUE)) {
    stop("'caminho_xlsx' deve ser um caminho terminado em .xlsx.", call. = FALSE)
  }
  if (!dir.exists(dirname(caminho))) {
    stop("A pasta de destino nao existe: ", dirname(caminho), call. = FALSE)
  }

  wb <- openxlsx::createWorkbook()

  # Aba 'resumo': dois blocos, um abaixo do outro
  bloco1 <- as.data.frame(relatorio$resumo_fisico_financeiro)
  names(bloco1) <- c("Ano", "Estoque_PCD (dez)", "Estoque_idoso  (dez)",
                     "Estoque_BPC  (dez)", "Total_PCD R$", "Total_Idoso R$",
                     "Total_BPC R$", "IC_inferior_BPC R$", "IC_Superior_BPC R$")
  bloco2 <- as.data.frame(relatorio$resumo_entradas_saidas)
  names(bloco2) <- c("Ano", "Entradas_adm_BPC", "Entradas_jud_BPC",
                     "Saidas_obito_BPC", "Saida_Revisao_BPC",
                     "Saidas_outras_BPC", "Resumo_Entradas", "Resumo_Saidas",
                     "Razao_S/E")

  openxlsx::addWorksheet(wb, "resumo")
  openxlsx::writeData(wb, "resumo", "RESUMO FISICO-FINANCEIRO",
                      startRow = 1, startCol = 1)
  openxlsx::writeData(wb, "resumo", bloco1, startRow = 2, startCol = 1)
  linha_bloco2 <- 2L + nrow(bloco1) + 2L
  openxlsx::writeData(wb, "resumo", "RESUMO ENTRADAS E SAIDAS (fisico)",
                      startRow = linha_bloco2, startCol = 1)
  openxlsx::writeData(wb, "resumo", bloco2,
                      startRow = linha_bloco2 + 1L, startCol = 1)

  openxlsx::addWorksheet(wb, "resumo_detalhes")
  openxlsx::writeData(wb, "resumo_detalhes",
                      .rotular_reais(relatorio$resumo_detalhes))

  openxlsx::addWorksheet(wb, "Planilha1")
  openxlsx::writeData(wb, "Planilha1", .rotular_reais(relatorio$planilha1))

  openxlsx::addWorksheet(wb, "Estoque_Mensal_Detalhado")
  openxlsx::writeData(wb, "Estoque_Mensal_Detalhado",
                      as.data.frame(relatorio$estoque_mensal_detalhado))

  openxlsx::saveWorkbook(wb, caminho, overwrite = TRUE)
  invisible(caminho)
}
