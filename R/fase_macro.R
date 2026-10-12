#' Coleta e Prepara Indicadores Conjunturais e Macro
#'
#' Obtém do Banco Central (SGS) o IPCA mensal e a taxa de desocupação e monta a
#' base macroeconômica mensal usada na projeção da fila.
#'
#' @details
#' \itemize{
#'   \item `ipca` está em **percentual** ao mês (série SGS 433), não em fração.
#'   \item `pop_idosa` é um valor constante (parâmetro `pop_idosa_base`); não há
#'     série histórica real incorporada.
#'   \item `taxa_informalidade` é uma aproximação linear da taxa de desocupação.
#'   \item Valores ausentes ao final das séries são preenchidos com o último
#'     valor observado.
#' }
#'
#' @param data_inicio Data inicial para a série temporal do BCB (formato
#'   "YYYY-MM-DD").
#' @param pop_idosa_base População idosa (constante) a ser usada na base.
#' @return Um tibble com os indicadores macroeconômicos mensais: `Data`, `ipca`,
#'   `taxa_desemprego`, `pop_idosa` e `taxa_informalidade`.
#' @examples
#' \donttest{
#' # Requer conexão com a internet
#' dt_macro <- obter_dados_macro()
#' }
#' @export
obter_dados_macro <- function(data_inicio = "2023-01-01",
                              pop_idosa_base = 32000000) {
  message("--- [FASE 0B] Mapeando indicadores conjunturais... ---")

  codigos_bcb <- c(ipca = 433, taxa_desemprego = 24369)

  series_bcb <- tryCatch(
    rbcb::get_series(codigos_bcb, start_date = data_inicio),
    error = function(e) {
      stop(
        "Nao foi possivel obter as series do Banco Central. ",
        "Verifique a conexao com a internet. Detalhe: ",
        conditionMessage(e),
        call. = FALSE
      )
    }
  )

  # Com um unico codigo o rbcb devolve um data.frame; com varios, uma lista.
  if (is.data.frame(series_bcb)) series_bcb <- list(series_bcb)

  dados_bcb <- Reduce(
    function(x, y) dplyr::left_join(x, y, by = "date"),
    series_bcb
  )

  if (nrow(dados_bcb) == 0L) {
    stop("O Banco Central nao retornou dados para o periodo solicitado.",
         call. = FALSE)
  }

  dt_macro <- dados_bcb %>%
    dplyr::rename(Data = "date") %>%
    dplyr::mutate(
      Data = .inicio_mes(Data),
      pop_idosa = pop_idosa_base,
      taxa_informalidade = 38.0 +
        (taxa_desemprego - mean(taxa_desemprego, na.rm = TRUE)) * 0.25
    ) %>%
    tidyr::fill(dplyr::everything(), .direction = "down")

  tibble::as_tibble(dt_macro)
}
