
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
#' @param values_from \code{\link[<tidy-select>]{dplyr::dplyr_data_masking}} of column in `data` to get cell values from.
#' @param values_to A string specifying the name of the new column created to store
#' the distributed values. Defaults to "value_new"
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
convert <- function(data, code_dict, code_from, code_to, weight_col, values_from, values_to = NULL){
  # ---- quosures ----
  # e_from <- rlang::parse_expr(code_from)
  # e_to <- rlang::parse_expr(code_to)
  # q_from <- rlang::new_quosure(e_from)
  # q_to <- rlang::new_quosure(e_to)
  q_weight <- rlang::enquo(weight_col)
  q_values <- rlang::enquo(values_from)
  # values_from // <tidy_select>

  # ---- input existence checks ----
  ### code_from exists in both data & code_dict
  assertthat::assert_that(has_name(data, code_from))
  assertthat::assert_that(has_name(code_dict, code_from))
  ### code_to exists in code_dict
  assertthat::assert_that(has_name(code_dict, code_to))

  ### values_from exists in data
  assertthat::assert_that(has_name(data, rlang::quo_name(q_values)))
  assertthat::assert_that(is.numeric(data[[rlang::quo_name(q_values)]]))
  ### values_to is given or created
  values_to %||% "value_new"
  ### weights are provided
  assertthat::assert_that(has_name(code_dict, rlang::quo_name(q_weight)))

  # ---- code_dict checks ----
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
  data_to <- dplyr::right_join(x = data,
                              y = code_dict,
                              by = code_from) %>%
    dplyr::mutate(weight_value = {{ weight_col }} * {{ values_from }}) %>%
    dplyr::group_by(.data[[code_to]], .add = TRUE) %>%
    dplyr::summarise({{ values_to }} := sum(weight_value))

  return(data_to)
}
