# Generated from _main.Rmd: do not edit by hand

#' Apply panel_map to data without checks
#'
#' A wrapper around a `{dplyr}` pipeline that takes a panel_map,
#' joins it with data, and transforms selected variables in that data according to
#' instructions in the panel map. Any groups in `data` are preserved.
#'
#' @param .data A data frame for transformation.
#' @param .map A data frame that contains a valid panel map for the transformation.
#' @param values_from Variable in `data` containing the values to be converted.
#' @param from_code A character string of the variable containing the codes to convert from.
#' Must be the same in `map` and `data`.
#' @param to_code Variable in `map` containing the codes to convert to.
#' @param weights Variable in `map` containing the weights to split values by.
#' @param .suffix A string appended to `value_from` column name to create column names for transformed values.
#' Defaults to `to_code`
#'
#' @param
#'
#' @return The output has the following properties:
#' * Groups are taken from `.data`
#'
#' @examples
use_panel_map <- function(.data, .map, from_code, to_code, weights,
                          .suffix = NULL){

  # convert from_code to string
  str_from_code <- as_string(enexpr(from_code))
  # subset data for transformation
  .data_in <- .data %>%
    dplyr::select({{from_code}}, {{values_from}})

  # merge map and data // use default by= argument
  map_join_data <- dplyr::right_join(x = .data_in,
                                     y = .map,
                                     by = str_from_code)

  # apply transformation
  .data_out <- map_join_data %>%
    dplyr::mutate(dplyr::across({{ values_from }}, ~ .x * {{ weights }})) %>%
    dplyr::group_by({{ to_code }}, .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ sum(.x)), .groups = "drop_last")

  # rename
  names_suffix <- .suffix %||% paste0("_", as_string(enexpr(to_code)))
  .data_out <- .data_out %>%
    dplyr::rename_with(., ~ paste0(.x, names_suffix), .cols = {{ values_from }})

  return(data_out)
}
