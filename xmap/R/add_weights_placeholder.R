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
#' @param frac_symbol The placeholder symbol to use for fractional weights. Default is "?".
#' @param unit_symbol The placeholder symbol to use for unit weights. Default is "".
#'
#' @return A data frame with a new column containing the weights placeholders.
#'
#' @importFrom dplyr group_by mutate ungroup n_distinct case_when
#' @importFrom rlang englue
#' @importFrom cli cli_inform
#'
#' @export
#' @examples
#' mock$xmap_abc |> as.data.frame() |>
#' add_weights_placeholder(from = upper, to = lower, unit_symbol = "1")
add_weights_placeholder <- function(df, from, to, weights_into = "weights_{{from}}", frac_symbol = "?", unit_symbol = ""){
  abort_dup_pairs(df, rlang::englue("{{from}}"), rlang::englue("{{to}}"))
  weights_into <- rlang::englue(weights_into)
  df |>
    dplyr::group_by({{from}}) |>
    dplyr::mutate("{weights_into}" := 1/dplyr::n_distinct({{to}})) |>
    dplyr::ungroup() -> w_df
  if(all(w_df[[weights_into]] == 1)){
    cli::cli_inform("No split relations found. Returning `df` with unit weights attached.")
    return(w_df)
  } else {
    w_df |>
      dplyr::mutate("{weights_into}" := case_when(.data[[weights_into]] < 1 ~ frac_symbol,
                                                  TRUE ~ unit_symbol))
  }
}
