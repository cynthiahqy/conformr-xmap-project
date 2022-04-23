
#' Convert data between standards
#'
#' @description
#' `convert()` maps numeric data from one category standard into another,
#' and verifies that total "amount" of data across one-to-one, many-to-one,
#' and one-to-many correspondences.
#'
#' Learn more in `vignette("convert")`
#'
#' @param data_map A data frame with data to be converted,
#' and correspondence and weights between codes, for every data value
#' @param code_in Variable in `data_map` containing the codes to convert from
#' @param code_out Variable in `data_map` containing the destination codes to convert to.
#' @param values_from `<tidy-select>` columns in `data-map` to get cell values from.
#' @param names_suffix A string appended to every `value_from` column name to create column names for transformed values.
#' Defaults to "_out"
#' @param weights Variable in `data_map` to get the
#' weights to distribute `values_from` between `code_in` and `code_out`
#' @return
#' @export
#'
#' @examples
#'
#'
convert <- function(data_map, code_in, code_out, weights, values_from,
                    names_suffix = "_out", .check = TRUE, .sum_na = TRUE){
  # ---- transform data ----
  data_out <- data_map %>%
    dplyr::mutate(dplyr::across({{ values_from }}, ~ .x * {{ weights }})) %>%
    dplyr::group_by({{ code_out }}, .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ sum(.x, na.rm = .sum_na)), .groups = "drop_last")

  # ---- check totals ----
  group <- data_map %>% dplyr::group_vars()
  totals_in <- data_map %>%
    dplyr::group_by({{ code_in }}, .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ mean(.x, na.rm = TRUE))) %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ sum(.x, na.rm = TRUE)))
  totals_out <- data_out %>%
    dplyr::summarise(dplyr::across({{ values_from }}, ~ sum(.x, na.rm = TRUE)))
  # total_check <- dplyr::full_join(x = totals_in,
  #                                 y = totals_out,
  #                                 by = group, suffix = c(".in", ".out"))

  # ---- correspondence checks ----
  ### value distribution weights total exactly 1 for each code_in (no value loss or creation)
  sum_weights <- data_map %>%
    dplyr::group_by({{ code_in }}, .add = TRUE) %>%
    dplyr::summarise(t_weight = sum({{ weights }}))

  if(.check == TRUE) {
    assertthat::assert_that(assertthat::are_equal(totals_in, totals_out))
    assertthat::assert_that(all(sum_weights$t_weight == 1))
  }

  # ---- rename data_out ----
  data_out <- data_out %>%
    dplyr::rename_with(., ~ paste0(.x, names_suffix), .cols = {{ values_from }})

  return(data_out)


}
