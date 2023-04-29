# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn abort Abort if named columns can't be found in df
#'
abort_missing_cols <- function(df, cols) {
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) != 0) {
    cli::cli_abort(
      message = "The column{?s} {.var {missing_cols}} {?was/were} not found.",
      class = "abort_missing_cols"
    )
  }
  invisible(df)
}

#' @describeIn abort Abort if xmap_df has missing values
#'
abort_any_na <- function(df) {
  if (base::anyNA(df)) {
    cli::cli_abort(
      message = "NA values found. Please enter missing `from` or `to` node labels and/or convert NA weights",
      class = "abort_na"
    )
  }
  invisible(df)
}

#' Validation functions and messages for xmap or candidate links (Internal)
#'
#' @description
#' Checks issues with data.frame like objects containing validated `xmap` or candidate links.
#'
#' @param df a data.frame-like object containing links
#' @param col_from,col_to,col_weights character vector or values naming columns from `df`
#'
#' @returns An error if the validation condition fails,
#' and invisibly returns `df` otherwise.
#'
#' @name abort
NULL

#' @describeIn abort Abort if xmap_df has wrong column types
#'
abort_weights_col_type <- function(df, col_weights) {
  if (!is.numeric(df[[col_weights]])) {
    cli::cli_abort(
      message = "The column `{col_weights}` should be of type numeric",
      class = "abort_col_type"
    )
  }
  invisible(df)
}

#' @describeIn abort Abort if duplicate source-target pairs are found
#'
abort_dup_pairs <- function(df, col_from, col_to) {
  if (!vhas_no_dup_pairs(df[[col_from]], df[[col_to]])) {
    cli::cli_abort(
      message = "Duplicate `from`-`to` links were found.
      Please remove or collapse duplicates.",
      class = "abort_dup_pairs"
    )
  }
  invisible(df)
}

#' @describeIn abort Abort for invalid mapping weights
#'
abort_bad_weights <- function(col_weights, call = rlang::caller_env()) {
    cli::cli_abort(
      message =  c(
            "Incomplete mapping weights found",
      "x" = "{.var {col_weights}} does not sum to 1",
      "i" = "Modify weights or adjust `tol` and try again."),
      class = "abort_bad_weights",
      call = call
    )
}

#' @describeIn abort Abort if xmap_df columns are not in order
abort_col_order <- function(df, col_from, col_to, col_weights){
  correct_order <- c(col_from, col_to, col_weights)
  first_three <- names(df[1:3])
  if(!identical(first_three, correct_order)){
    rlang::abort(
      message = "columns are not sorted in order `from`, `to`, `weights`",
      class = "abort_col_order"
    )
  }
  invisible(df)
}

#' @describeIn abort Abort if from_set attribute doesn't match xmap_df values
#'
abort_from_set <- function(df, col_from, from_set) {
  col_from_set <- as.character(unique(df[[col_from]]))
  stopifnot(identical(col_from_set, from_set))

  invisible(df)
}

#' @describeIn abort Abort message for fractional weights
#' @export
msg_abort_frac_weights <- function(impact){
  cli::format_error(c(
            "`x` contains fractional weights. {impact}",
      "x" = "You've supplied a xmap with weights not equal to 1")
    )
}

#' @describeIn abort Abort if xmap_df is not reversible without new weights
#'
abort_not_reversible <- function(df, col_to) {
  x_to <- df[[col_to]]
  if (vhas_collapse(x_to)){
    cli::cli_abort("Collapse links in {.var xmap_df} cannot be reversed. Please supply new weights and create a new xmap.")
  }
  invisible(df)
}
  
