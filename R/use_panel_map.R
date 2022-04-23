# TODO: see also archive-convert.R

#' Use a panel map to transform data
#'
#' A light wrapper around a `{dplyr}` pipeline that takes a valid panel map,
#' joins it with data, and transforms selected variables in that data according to
#' instructions in the panel map. Any groups in `data` are preserved.
#'
#' @param map A data frame that contains a valid panel map for the transformation.
#' @param data A data frame for transformation.
#' @param values_from Variables in `data` containing the values to be converted.
#' @param code_from A character string of the variable containing the codes to convert from.
#' If the variables containing the source codes differ in `map` and `data`, use a named vector.
#' For example, `from_code = c("a" = "b")` will match codes in `map$a` to `data$b`.
#' @param to_code Variable in `map` containing the codes to convert to.
#' @param weights Variable in `map` containing the weights to split values by.
#'
#' @param
#'
#' @return The output has the following properties:
#' * Groups are taken from `data_from`
#' @export
#'
#' @examples
use_panel_map <- function(map, data, values_from,
                          from_code = NULL, to_code = NULL, weights = NULL){
  # check validity of map
  abort(!is_panel_map(map))

  # default values


  # merge map and data
  data_from <- data %>%
    dplyr::select(from_code, {{values_from}})

  code_in <- names(from_code)

  map_join_data <- dplyr::right_join(x = data_from,
                                     y = map,
                                     by = code_in)

  # apply transformation
  data_out <- map_join_data %>%
    dplyr::mutate(dplyr::across({{ values_from }}, ~ .x * {{ weights }})) %>%
    dplyr::group_by({{ to_code }}, .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ sum(.x)), .groups = "drop_last")

  return(data_out)
}
