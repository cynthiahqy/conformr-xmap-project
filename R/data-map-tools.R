# Completeness Checks

#' Verify data map weights
#'
#' Check that weights applied to given code_in/value_from add up to 1.
#' Ensures that no value is created or lost in the transformation process.
#' Each `value_from` numeric will be split into parts totalling 100%
#'
#' NA values of weights will (cause error/be fixed?)
#'
#' @param data_map
#' @param code_in
#' @param code_out
#'
#' @return
#' @export
#'
#' @family  data map tools
#' @seealso [dm_check_values()], [dm_check_codes()]
#'
#' @examples
dm_check_weights <- function(data_map, code_in, code_out, na = c('rm', 'zero')){

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
