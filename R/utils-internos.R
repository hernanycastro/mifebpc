# Funcoes internas (nao exportadas)

# Declaração de variáveis globais para evitar notas no R CMD check (Non-Standard Evaluation do dplyr)
if (getRversion() >= "2.15.1") {
  utils::globalVariables(c(
    "Volume_Fisico_Equivalente",
    "Impacto_Variacao_Fisico",
    "Impacto_Variacao_SM",
    "Custo_Total",
    "Volume_Fisico_Real",
    "Ano",
    "SM",
    "Data",
    "especie",
    "Entrada_Adm",
    "Entrada_Jud",
    "TMC_Adm_Meses",
    "TMC_Jud_Meses"
  ))
}

# Normaliza qualquer data (Date ou POSIXct) para o primeiro dia do mes.
# Garante que series do BCB, Excel e CSV sejam cruzadas pela mesma chave.
.inicio_mes <- function(x) {
  as.Date(format(as.Date(x), "%Y-%m-01"))
}

# Verifica se um arquivo existe. system.file() devolve "" quando o arquivo nao
# esta em inst/extdata/, o que gera erros pouco claros mais adiante.
.checar_arquivo <- function(caminho, descricao) {
  invalido <- !is.character(caminho) || length(caminho) != 1L ||
    is.na(caminho) || !nzchar(caminho) || !file.exists(caminho)
  if (invalido) {
    stop(
      "Arquivo nao encontrado: ", descricao, ". ",
      "Verifique se ele esta em inst/extdata/ (e reinstale o pacote) ",
      "ou informe o caminho manualmente.",
      call. = FALSE
    )
  }
  invisible(caminho)
}

# Verifica se um data.frame tem as colunas esperadas.
.checar_colunas <- function(df, colunas, nome) {
  faltantes <- setdiff(colunas, names(df))
  if (length(faltantes) > 0L) {
    stop(
      "Colunas ausentes em ", nome, ": ",
      paste(faltantes, collapse = ", "),
      call. = FALSE
    )
  }
  invisible(df)
}
