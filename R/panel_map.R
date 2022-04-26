# ---- Construction ----
#' Low level constuctor for tbl_pm
#'
#' @param x Data frame containing the following variables
#' @param code_in Variable that specifies the source classification categories.
#' @param code_out Variable that specifies the target classification categories.
#' @param split_in Variable that specifies the weights to apply to source values.
#' @param by Variables(s) that specify row grouping for "sub" panel maps.
#' For example, `code_in = "ISICv4", by = "country"` retains `x$country`
#' and `x$ISICv4` in the panel_map.
#'
#' @return A `panel_map` object
#' @author Cynthia Huang
new_panel_map <- function(x, code_in, code_out, split_in, by = NA){

  x_pm <- try(dplyr::select(x,
                            {{code_in}}, {{code_out}},
                            {{split_in}}, {{by}}))

  tbl_pm <- tibble::new_tibble(x_pm,
                               from = as_string(enexpr(code_in)),
                               to = as_string(enexpr(code_out)),
                               weights = as_string(enexpr(split_in)),
                               #by = as_string(enexpr(by)),
                               class = c("panel_map"))

  tbl_pm
}

#' Test if the object is a panel_map
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

# ---- Validation ----
#' A Validator for `panel_map` objects
#'
#' @param pm
#'
#' @return
#' @export
#'
#' @examples
validate_panel_map <- function(x){
  x <- unclass(x)
}

check_pm_split_sum <- function(x, code_in, split_in){
  x %>%
    dplyr::group_by(code_in) %>%
    dplyr::summarise(split_total = sum(split_in))
}

check_pm_NA <- function(pm){

}

check_pm_duplicates <- function(pm){

}

# ---- Errors ----
error_incomplete_split <- function(pm){

}

# ---- Helpers ----
#' Create a panel_map from a data frame like object
#'
#' @param x Data frame
#' @param code_in Variable in `x` containing the source classification codes to convert from.
#' @param code_out Variable in `x` containing the destination classification codes to convert to.
#' @param split_in Variable in `x` containing the transformation weights to apply
#' to source data for each source code.
#'
#' @return
#' @export
#'
#' @examples
#' panel_map
as_panel_map <- function(x, code_in, code_out, split_in){
  ## TODO: implement internal pm class
  pm <- to_pm(x, code_in, code_out, split_in)

  ## TODO: validation
  check_pm_duplicates(pm)   # TRUE iff distinct() doesn't reduce no. of rows
  check_pm_NA(pm)           # FALSE if NA are found, they must be converted to 0?
  check_pm_split_sum(pm)    # TRUE iff split_in sums to 1 // FALSE if NA are found

  ## TODO: error messages
}

