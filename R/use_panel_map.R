# TODO: see also archive-convert.R

#' Check and apply panel_map transformation to data
#'
#' @param map
#' @param data
#' @param values_from
#' @param code_from
#'
#' @return
#' @export
#'
#' @examples
transform_data_by_map <- function(map, data, values_from, code_from,
                                  .validate_coverage = TRUE){
  # ---- check map class ----
  abort(!is_panel_map(map))

  if (.validate_coverage) {

  }





}

#' Apply panel_map to data without checks
#'
#' A wrapper around a `{dplyr}` pipeline that takes a panel_map,
#' joins it with data, and transforms selected variables in that data according to
#' instructions in the panel map. Any groups in `data` are preserved.
#'
#' @param map A data frame that contains a valid panel map for the transformation.
#' @param data A data frame for transformation.
#' @param value_from Variable in `data` containing the values to be converted.
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
#' * Groups are taken from `data_from`
#' @export
#'
#' @examples
use_panel_map <- function(map, data, value_from, from_code, to_code, weights,
                          .suffix = NULL){

  # convert from_code to string
  from_code <- as_string(enexpr(from_code))
  # subset data for transformation
  data_in <- data %>%
    dplyr::select(from_code, {{value_from}})

  # merge map and data // use default by= argument
  map_join_data <- dplyr::right_join(x = data_in,
                                     y = map,
                                     by = from_code)

  # apply transformation
  data_out <- map_join_data %>%
    dplyr::mutate(dplyr::across({{ value_from }}, ~ .x * {{ weights }})) %>%
    dplyr::group_by({{ to_code }}, .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ value_from }}, ~ conformr::sum_greedy(.x)), .groups = "drop_last")

  # rename
  names_suffix <- .suffix %||% paste0("_", as_string(enexpr(to_code)))
  data_out <- data_out %>%
    dplyr::rename_with(., ~ paste0(.x, names_suffix), .cols = {{ value_from }})

  return(data_out)
}

#' Check code-dict coverage
#'
#' Checks if every `code_in` in `data` has
#' at least one destination (row) in `panel_map`.
#' Warns if any `data$code_in` is not present in `panel_map$code_in`
#'
#' @param data <df> with data to be converted. Must contain `code_in` column.
#' @param map <panel_map>
#' @param code_in A named character vector specifying the columns in `data` and `map`
#'
#' @return
#' @export
#'
#' @family code_dict helpers
#'
#' @examples
check_cd_coverage <- function(map, data, code_in){

  ### every code in data has a destination instruction in code_dict
  # unique codes
  data_codes <- sort(unique(data[[code_in]]))
  cd_codes <- sort(unique(code_dict[[code_in]]))

  # coverage checks
  ## at least one instruction
  covered <- data_codes %in% cd_codes
  n_not_covered <- sum(!covered)

  # messages
  if (all(covered)){
    rlang::inform(message = c("All unique `data$code_in` are found in `code_dict$code_in`"))
  } else {
    rlang::warn(message = c("Can't find some `data$code_in` in `code_dict$code_in`:")
    )
  }
}
