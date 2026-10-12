#' @title Modulo de Consulta e Inspecao de Dados e Parametros
#' @name fase_consulta
NULL

#' Consulta o Historico de Fila Processada do BPC
#'
#' Le e filtra o arquivo historico de fila processada armazenado no pacote.
#'
#' @param especie Caractere opcional para filtrar por especie ("87" para PCD, "88" para Idoso, ou `NULL` para ambas).
#' @param data_inicio Data inicial no formato "AAAA-MM-DD" ou `NULL` para ignorar.
#' @param data_fim Data final no formato "AAAA-MM-DD" ou `NULL` para ignorar.
#' @return Um `tibble` contendo o historico filtrado.
#' @export
#' @examples
#' \dontrun{
#'   df_hist_fila <- consultar_historico_fila(especie = "87", data_inicio = "2024-01-01")
#' }
consultar_historico_fila <- function(especie = NULL, data_inicio = NULL, data_fim = NULL) {
  caminho <- system.file("extdata", "bpc_historico_fila_processado.xlsx", package = "mifebpc")
  .checar_arquivo(caminho, "Historico de Fila Processada do BPC")

  df <- readxl::read_excel(caminho)

  if ("Data" %in% names(df)) {
    df$Data <- as.Date(df$Data)
  }

  if (!is.null(especie) && "especie" %in% names(df)) {
    df <- dplyr::filter(df, especie == as.character(especie))
  }

  if (!is.null(data_inicio) && "Data" %in% names(df)) {
    df <- dplyr::filter(df, Data >= as.Date(data_inicio))
  }

  if (!is.null(data_fim) && "Data" %in% names(df)) {
    df <- dplyr::filter(df, Data <= as.Date(data_fim))
  }

  tibble::as_tibble(df)
}

#' Consulta o Historico Agregado de Quadrantes do BPC
#'
#' Le e retorna o historico agregado de quadrantes armazenado no pacote.
#'
#' @return Um `tibble` contendo o historico agregado.
#' @export
#' @examples
#' \dontrun{
#'   df_quadrantes <- consultar_historico_quadrantes()
#' }
consultar_historico_quadrantes <- function() {
  caminho <- system.file("extdata", "bpc_historico_agregado_quadrantes.csv", package = "mifebpc")
  .checar_arquivo(caminho, "Historico Agregado de Quadrantes do BPC")

  # Utiliza fill = TRUE para acomodar linhas com campos finais ausentes sem emitir avisos
  df <- data.table::fread(caminho, encoding = "UTF-8", fill = TRUE)
  tibble::as_tibble(df)
}

#' Consulta os Parametros de Expectativa de Revisao
#'
#' Inspeciona o arquivo de expectativa de revisoes cadastrais nativo do pacote.
#'
#' @return Um `tibble` com as expectativas de revisao por periodo/especie.
#' @export
#' @examples
#' \dontrun{
#'   df_rev <- consultar_parametros_revisao()
#' }
consultar_parametros_revisao <- function() {
  caminho <- system.file("extdata", "expectativa_revisao.xlsx", package = "mifebpc")
  .checar_arquivo(caminho, "Expectativa de Revisao")

  df <- readxl::read_excel(caminho)
  tibble::as_tibble(df)
}

#' Consulta os Parametros da Tabua Biometrico-Atuarial
#'
#' Inspeciona os dados da tabua anualizada utilizada nas projecoes de obito.
#'
#' @param ano_filtro Inteiro opcional para filtrar um ano especifico da tabua.
#' @return Um `tibble` com a estrutura da tabua biometrica.
#' @export
#' @examples
#' \dontrun{
#'   df_tabua <- consultar_parametros_tabua(ano_filtro = 2026)
#' }
consultar_parametros_tabua <- function(ano_filtro = NULL) {
  caminho <- system.file("extdata", "tabua_anualizada.xlsx", package = "mifebpc")
  .checar_arquivo(caminho, "Tabua Anualizada")

  df <- readxl::read_excel(caminho)

  if (!is.null(ano_filtro) && "Ano" %in% names(df)) {
    df <- dplyr::filter(df, Ano == as.integer(ano_filtro))
  }

  tibble::as_tibble(df)
}

#' Consulta os Valores Executados Historicos do BPC
#'
#' Le e filtra o arquivo de historico financeiro executado armazenado no pacote.
#'
#' @param data_inicio Data inicial no formato "AAAA-MM-DD" ou `NULL` para ignorar.
#' @param data_fim Data final no formato "AAAA-MM-DD" ou `NULL` para ignorar.
#' @return Um `tibble` contendo os valores executados por competencia.
#' @export
#' @examples
#' \dontrun{
#'   df_exec <- consultar_valores_executados(data_inicio = "2026-01-01")
#' }
consultar_valores_executados <- function(data_inicio = NULL, data_fim = NULL) {
  caminho <- system.file("extdata", "executado.xlsx", package = "mifebpc")
  .checar_arquivo(caminho, "Valores Executados do BPC")

  df <- readxl::read_excel(caminho, sheet = "resumo_financeiro")

  if ("Ano" %in% names(df)) {
    df$Ano <- as.Date(df$Ano)
  }

  if (!is.null(data_inicio) && "Ano" %in% names(df)) {
    df <- dplyr::filter(df, Ano >= as.Date(data_inicio))
  }

  if (!is.null(data_fim) && "Ano" %in% names(df)) {
    df <- dplyr::filter(df, Ano <= as.Date(data_fim))
  }

  tibble::as_tibble(df)
}

#' Resumo Geral dos Metadados e Parametros Vigentes
#'
#' Retorna uma lista com o panorama dos arquivos e parametros disponiveis no pacote,
#' garantindo a seguranca ao omitir caminhos absolutos do sistema de arquivos.
#'
#' @return Uma lista nomeada com os sumarios de configuracao.
#' @export
#' @examples
#' \dontrun{
#'   resumo <- listar_metadados_pacote()
#' }
listar_metadados_pacote <- function() {
  list(
    versao_pacote = utils::packageVersion("mifebpc"),
    bases_disponiveis = c(
      "fila_processada",
      "quadrantes",
      "expectativa_revisao",
      "tabua_anualizada",
      "executado"
    ),
    descricao = "Modulo de consulta integrado para auditoria, inspecao de dados base e comparacao de valores executados."
  )
}
