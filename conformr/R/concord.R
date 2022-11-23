# Generated from _main.Rmd: do not edit by hand

#' Title
#'
#' @param .data_in A data frame containining `from_code` and 
#' @param .map 
#' @param from_code 
#' @param to_code 
#' @param m_weights
#' @param ... variables to transform
#'
#' @return
#' @export
#'
#' @examples
concord <- function(.data_in, .map, from_code, to_code, m_weights, ...){
  
  ## wrangle inputs
  dots <- rlang::enquos(..., .named = TRUE)
  
  ## check conditions
  
  in_data_in <- (names(dots) %in% colnames(.data_in))
  if (!all(in_data_in)){
    cli::cli_abort(
      "{.code {names(dots)[!in_data_in]}} cannot be found in {.var .data_in}",
      class = "cols_not_found")
  }
  
  .data_in |>
    check_missing()
  .map |> 
    check_weights()
  
  has_coverage()
  
  ## apply transformation
  
}
