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
use_panel_map <- function(map, data, values_from, code_from){
  # check argument types
  ## abort(!is_panel_map(map))

  # match map$code_in and data$code_from
  code_in <- attr(map, "from", exact = TRUE)
  code_from <- as_string(enexpr(code_from))


  data_from <- data %>%
    dplyr::rename(code_in = code_from) %>%
    dplyr::select(code_from, {{values_from}}) %>%
    # pivot_longer to turn values_from into single column
    tidyr::pivot_longer(., {{values_from}}, names_to = "from_name", values_to = "value_in")

  # merge map and data // use default by= argument
  map_join_data <- dplyr::right_join(x = data_from,
                                     y = map)

  # apply transformation
  data_out <- map_join_data %>%
    dplyr::mutate(dplyr::across({{ values_from }}, ~ .x * {{ weights }})) %>%
    dplyr::group_by({{ to_code }}, .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ sum(.x)), .groups = "drop_last")

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
