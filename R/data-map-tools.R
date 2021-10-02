# Completeness Checks

#' Verify data/code map weights
#'
#' Check that weights applied to given code_in/value_from add up to 1.
#' Ensures that no value is created or lost in the transformation process.
#' Each `value_from` numeric will be split into parts totalling 100%
#'
#' NA values of weights will (cause error/be fixed?)
#'
#' @param map code or data map with correspondence between code_in and code_out
#' @param code_in
#' @param code_out
#' @param weights
#' @param ... named arguments passed to assertr fncs.
#' Specifically `success_fun` and `error_fun` for modifying function output
#'
#' @return map
#' @export
#'
#' @family  data map tools
#' @seealso [dm_check_values()], [dm_check_codes()]
#'
#' @examples
dm_check_weights <- function(map, code_in, code_out, weights, ...){
# assertr params
  params <- list(...)
  params$success_fun <- params$success_fun %||% assertr::success_logical
  params$error_fun <- params$error_fun %||% assertr::error_logical

# calculate and check total weights
  t_weight <- map %>%
    dplyr::group_by({{ code_in }}, .add = TRUE) %>%
    dplyr::mutate(total_in_weight = sum({{ weights }})) %>%
    dplyr::ungroup()

# write predicate
  equal_one <- function(x) if (x != 1) return(FALSE)
# check total using assertr
  outcome <- assertr::assert(data = t_weight,
                             predicate = equal_one,
                             total_in_weight,
                             success_fun = params$success_fun,
                             error_fun = params$error_fun)

# return data_map or error report
  return(outcome)
}


#' Verify data map code correspondence
#'
#' Check that every unique `code_in` has a `code_out`
#'
#' Returns warning for missing `code_in`, `code_out` values,
#' with suggested solutions.
#'
#' @param data_map
#' @param code_in
#' @param code_out
#'
#' @return
#' @export
#'
#' @family  data map tools
#' @seealso [dm_check_weights()], [dm_check_values()]
#'
#' @examples
dm_check_codes <- function(data_map, code_in, code_out){

}

#' Verify data map values
#'
#' Check that every `code_in`-`code_out` correspondence has appropriate `values_from`
#' to apply `weights` to.
#' In particular, check that one-to-many splits have `values_from` duplicated across
#' all `code_in`-`code_out`-`weight` rows within a particular `code_in`
#'
#' @param data_map
#' @param code_in
#' @param code_out
#' @param values_from
#'
#' @family  data map tools
#' @seealso [dm_check_codes()], [dm_check_weights()]
#'
#' @return
#' @export
#'
#' @examples
dm_check_values <- function(data_map, code_in, code_out, weights, values_from){

}
