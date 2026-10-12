#' Calcula Impactos Financeiros e Decomposição de Variações
#'
#' @details
#' \itemize{
#'    \item Anos da projeção sem salário mínimo em `tabela_sm` são **excluídos**
#'      da tabela de impactos anuais (com aviso), em vez de gerar custo zero.
#'    \item O primeiro ano da série tem os impactos de variação definidos como `NA`,
#'      pois não possui ano anterior para base de comparação incremental.
#'    \item `Impacto_Fisico` mantém a fórmula original:
#'      `(Entrada_Adm + Entrada_Jud) * (max(TMC_Adm - 1, 0) + max(TMC_Jud - 1, 0))`.
#' }
#'
#' @param df_fluxo_detalhado Data frame gerado por [executar_motor_atuarial()].
#' @param taxa_juros Taxa de juros anual para ajuste do custo total.
#' @param tabela_sm Data frame com as colunas `Ano` e `SM` (salário mínimo).
#' @return Uma lista com `resumo_mensal` (fluxo mensal em formato largo) e
#'    `impactos_var` (custo anual e decomposição rigorosa das variações).
#' @export
calcular_impactos_financeiros <- function(
    df_fluxo_detalhado,
    taxa_juros = 0.0403,
    tabela_sm = tibble::tibble(
      Ano = 2026:2031,
      SM = c(1621, 1741, 1854, 1956, 2066, 2180)
    )
) {
  message("--- [FASE 5] Calculando impactos financeiros e decomposicoes ---")

  if (missing(df_fluxo_detalhado) || !is.data.frame(df_fluxo_detalhado)) {
    stop("'df_fluxo_detalhado' deve ser o resultado de executar_motor_atuarial().",
         call. = FALSE)
  }

  variaveis <- c("Estoque_Ativo", "Entrada_Adm", "Entrada_Jud",
                 "TMC_Adm_Meses", "TMC_Jud_Meses", "Saida_Obito",
                 "Saida_Revisao", "Saida_Residual")
  .checar_colunas(df_fluxo_detalhado, c("Data", "especie", variaveis),
                  "df_fluxo_detalhado")
  .checar_colunas(tabela_sm, c("Ano", "SM"), "tabela_sm")

  df_processado <- df_fluxo_detalhado %>%
    dplyr::mutate(Ano = as.integer(format(Data, "%Y"))) %>%
    dplyr::left_join(tibble::as_tibble(tabela_sm), by = "Ano")

  # --- Resumo mensal em formato largo ---------------------------------------
  cols_ordem <- c(
    "Data",
    as.vector(rbind(paste5 <- paste0(variaveis, "_PCD"), paste0(variaveis, "_IDOSO"))) # ajustado para compatibilidade
  )
  # Nota: mantendo a montagem original limpa das colunas
  cols_ordem <- c(
    "Data",
    as.vector(rbind(paste0(variaveis, "_PCD"), paste0(variaveis, "_IDOSO")))
  )

  resumo_mensal <- df_processado %>%
    tidyr::pivot_wider(
      id_cols = c(Data, Ano, SM),
      names_from = especie,
      values_from = dplyr::all_of(variaveis)
    ) %>%
    dplyr::select(dplyr::all_of(cols_ordem))

  # --- Impactos anuais com tratamento rigoroso para o primeiro ano ---
  anos_sem_sm <- sort(unique(df_processado$Ano[is.na(df_processado$SM)]))
  if (length(anos_sem_sm) > 0L) {
    warning(
      "Sem salario minimo em 'tabela_sm' para o(s) ano(s): ",
      paste(anos_sem_sm, collapse = ", "),
      ". Esses periodos foram excluidos de 'impactos_var'.",
      call. = FALSE
    )
  }

  df_impactos_var <- df_processado %>%
    dplyr::filter(!is.na(SM)) %>%
    dplyr::mutate(
      Impacto_Fisico = (Entrada_Adm + Entrada_Jud) *
        (pmax(TMC_Adm_Meses - 1, 0) + pmax(TMC_Jud_Meses - 1, 0))
    ) %>%
    dplyr::group_by(Ano) %>%
    dplyr::summarise(
      Meses_Ano = dplyr::n(),
      Custo_Total = sum(Impacto_Fisico * SM * (1 + taxa_juros), na.rm = TRUE),
      Volume_Fisico_Real = sum(Impacto_Fisico, na.rm = TRUE),
      Volume_Fisico_Equivalente = mean(Impacto_Fisico, na.rm = TRUE) * 12,
      Valor_SM = mean(SM, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      # Decomposição exata
      Impacto_Variacao_Fisico = (Volume_Fisico_Equivalente - dplyr::lag(Volume_Fisico_Equivalente)) * Valor_SM,
      Impacto_Variacao_SM     = (Valor_SM - dplyr::lag(Valor_SM)) * dplyr::lag(Volume_Fisico_Equivalente),

      # Garante explicitamente que o primeiro ano da série fique com NA nos impactos incrementais
      Impacto_Variacao_Fisico = dplyr::if_else(dplyr::row_number() == 1L, NA_real_, Impacto_Variacao_Fisico),
      Impacto_Variacao_SM     = dplyr::if_else(dplyr::row_number() == 1L, NA_real_, Impacto_Variacao_SM)
    ) %>%
    dplyr::select(
      Ano,
      Custo_Total,
      Volume_Fisico_Equivalente,
      Volume_Fisico_Real,
      Valor_SM,
      Impacto_Variacao_Fisico,
      Impacto_Variacao_SM
    )

  list(resumo_mensal = resumo_mensal, impactos_var = df_impactos_var)
}
