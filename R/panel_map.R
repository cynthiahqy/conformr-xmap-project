#' Build a panel.map from a data frame
#'
#' @param x A data frame.
#' @param code_in Variable in `x` containing the source classification codes to convert from.
#' @param code_out Variable in `x` containing the destination classification codes to convert to.
#' @param split_in Variable in `x` containing the transformation weights to apply
#' to source data for each source code.
#'
#' @return returns TRUE or FALSE depending
#' @export
#'
#' @examples
panel_map <- function(x, code_in, code_out, split_in){
  ## TODO: implement internal pm class
  pm <- to_pm(x, code_in, code_out, split_in)

  ## TODO: prioritise checks
  check_pm_duplicates(pm)   # TRUE iff distinct() doesn't reduce no. of rows
  check_pm_NA(pm)           # FALSE if NA are found, they must be converted to 0?
  check_pm_split_sum(pm)    # TRUE iff split_in sums to 1 // FALSE if NA are found

  ## TODO: error messages
}

#' Test if the object is a panel.map
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
is_panel_map <- function(x){
  inherits(x, "pm_df")
}
