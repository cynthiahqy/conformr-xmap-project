
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
#' the distributed values.
#' @param weights A string specifying which column in `code_dict` to get the
#' weights to distribute `values_from` between `code_from` and `code_to`
#' @return
#' @export
#'
#' @examples
convert <- function(data, code_dict, code_from, code_to, values_from, values_to, weights){
  # ---- input checks
  assertthat::assert_that(has_name(data, code_from) & has_name(code_dict, code_from))

}
