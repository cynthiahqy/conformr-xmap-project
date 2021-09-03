
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
#' @param values_from `<tidy-select>` columns in `data` to get cell values from.
#' @param names_suffix A string appended to every `value_from` column name to create column names for transformed values.
#' Defaults to "_out"
#' @param weight_col A string specifying which column in `code_dict` to get the
#' weights to distribute `values_from` between `code_from` and `code_to`
#' @return
#' @export
#'
#' @importFrom assertthat has_name
#' @importFrom rlang .data
#' @examples
#'
#'
convert <- function(data, code_dict, code_from, code_to, weight_col, values_from, names_suffix = "_out"){
  # ---- quosures ----
  q_weight <- rlang::enquo(weight_col)
  q_values <- rlang::enquo(values_from)

  # ---- input existence checks ----
  ### code_from exists in both data & code_dict
  assertthat::assert_that(has_name(data, code_from))
  assertthat::assert_that(has_name(code_dict, code_from))
  ### code_to exists in code_dict
  assertthat::assert_that(has_name(code_dict, code_to))

  ### values_from exists in data
  # assertthat::assert_that(has_name(data, rlang::quo_name(q_values)))
  # assertthat::assert_that(is.numeric(data[[rlang::quo_name(q_values)]]))
  ### weights are provided
  assertthat::assert_that(has_name(code_dict, rlang::quo_name(q_weight)))

  # ---- code_dict checks ---- //
  # TODO: replace with code-dict-tools fncs
  ### no duplicate instruction in code_dict // probably safe to correct inside fnc
  assertthat::assert_that(nrow(code_dict) == nrow(dplyr::distinct(code_dict)))
  ### every code in data has a destination instruction in code_dict
  assertthat::assert_that(all(unique(data[code_from]) %in% unique(code_dict[code_from])))
  ### value distribution weights total exactly 1 for each code_from (no value loss or creation)
  t_weight_by_code_from <- code_dict %>%
    dplyr::group_by(.data[[code_from]]) %>%
    dplyr::summarise(t_weight = sum({{ weight_col }}))
  assertthat::assert_that(all(t_weight_by_code_from$t_weight == 1))

  # ---- transform data ----
  data_out <- dplyr::right_join(x = data, y = code_dict, by = code_from) %>%
    dplyr::mutate(dplyr::across({{ values_from }}, ~ .x * {{ weight_col }})) %>%
    dplyr::group_by(.data[[code_to]], .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ sum(.x), .names = paste0("{.col}", names_suffix)))

  return(data_out)
}
