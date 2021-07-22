
#' Convert data between standards
#'
#' @description
#' `convert()` maps numeric data from one category standard into another.
#' It maintains the total "amount" of data across one-to-one, many-to-one,
#' and one-to-many correspondences.
#'
#' Learn more in `vignette("convert")`
#'
#' @param data A data frame to convert. Can be grouped data.
#' @param code_dict A data frame with code correspondence between standards.
#' @param code_from A string specifying the shared column name in `data` and `code_dict`
#' with the codes to convert from.
#' @param code_to A string specifying which column in `code_dict` contains the
#' destination codes to convert to.
#' @param values_from A string specifying which column in `data` to get cell values from.
#' @param values_to A string specifying the name of the new column created to store
#' the distributed values. Defaults to "value_new"
#' @param weights A string specifying which column in `code_dict` to get the
#' weights to distribute `values_from` between `code_from` and `code_to`
#' @return
#' @export
#'
#' @examples
convert <- function(data, code_dict, code_from, code_to, values_from, values_to = NULL, weights){
  # ---- input existence checks ----
  ### code_from exists in both data & code_dict
  assertthat::assert_that(has_name(data, code_from))
  assertthat::assert_that(has_name(code_dict, code_from))
  ### code_to exists in code_dict
  assertthat::assert_that(has_name(code_dict, code_to))

  ### values_from exists in data
  assertthat::assert_that(has_name(data, values_from))
  ### values_to is given or created
  values_to %||% "value_new"
  ### weights are provided
  assertthat::assert_that(has_name(code_dict, weights))

  # ---- code_dict checks ----
  ### no duplicate instruction in code_dict // probably safe to correct inside fnc
  assertthat::assert_that(nrow(code_dict) == nrow(distinct(code_dict)))
  ### every code in data has a destination instruction in code_dict
  assertthat::assert_that(all(unique(data[code_from]) %in% unique(code_dict[code_from])))
  ### value distribution weights total exactly 1 for each code_from (no value loss or creation)
  t_weight_by_code_from <- code_dict %>%
    dplyr::group_by(!!sym(code_from)) %>%
    dplyr::summarise(t_weight = sum(!!sym(weights)))
  assertthat::assert_that(all(t_weight_by_code_from$t_weight == 1))

}
