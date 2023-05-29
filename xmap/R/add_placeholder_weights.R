# Generated from create-xmap.Rmd: do not edit by hand

#' Add weights placeholder column to a data frame
#'
#' This function adds a column of character placeholders to a data frame based on
#' the values of `from` and `to` columns. 1-to-1 links are given unit weight placeholders,
#' and 1-to-many links are given fractional weight placeholders.
#'
#' @param df A data frame.
#' @param from The name of the column containing source category values.
#' @param to The name of the column containing target category values.
#' @param weights_into The name to use for the column containing the weights
#'   placeholders. Default is "weights_{{from}}".
#'
#' @return A data frame with a new numeric column containing the weights placeholders.
#'
#' @importFrom dplyr group_by mutate ungroup n_distinct case_when
#' @importFrom rlang englue
#' @importFrom cli cli_inform
#'
#' @seealso [add_weights] to generate valid crossmap weights.
#'
#' @export
#' @examples
#' mock$xmap_abc |>
#'   as.data.frame() |>
#'   add_placeholder_weights(from = upper, to = lower)
add_placeholder_weights <- function(df, from, to, weights_into = "weights_{{from}}") {
  abort_dup_pairs(df, rlang::englue("{{from}}"), rlang::englue("{{to}}"))
  
  ## set up
  weights_into <- rlang::englue(weights_into)
  frac_symbol <- NA
  unit_symbol <- 1
  ## equal weights
  df |>
    dplyr::group_by({{ from }}) |>
    dplyr::mutate("{weights_into}" := 1 / dplyr::n_distinct({{ to }})) |>
    dplyr::ungroup() -> w_df
  
  if (all(w_df[[weights_into]] == 1)) {
    #cli::cli_inform("No split relations found. Returning `df` with unit weights attached.")
    return(w_df)
  } else {
    w_df |>
      dplyr::mutate("{weights_into}" := case_when(
        .data[[weights_into]] == 1 ~ unit_symbol,
        .data[[weights_into]] < 1 ~ frac_symbol,
        TRUE ~ frac_symbol
      ))
  }
}
