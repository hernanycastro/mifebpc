#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
NULL

# Variaveis usadas via avaliacao nao-padrao (dplyr). Mantidas aqui em um unico
# lugar para evitar NOTEs do R CMD check.
utils::globalVariables(c(
  # fila / macro
  "Data", "data_mife", "ipca", "taxa_desemprego", "pop_idosa",
  "taxa_informalidade",
  # atuarial
  "Ano", "CS_ESPECIE", "idade", "q_x", "n_pessoas_real",
  # financeira
  "SM", "especie", "Entrada_Adm", "Entrada_Jud", "TMC_Adm_Meses",
  "TMC_Jud_Meses", "Impacto_Fisico", "Volume_Fisico", "Valor_SM"
))
