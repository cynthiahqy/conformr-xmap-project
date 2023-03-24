# Generated from create-xmap.Rmd: do not edit by hand

#' xmap_df validation helpers
#'
#' @description
#' Checks issues with an xmap_df object.
#'
#' @param df a data.frame-like object containing an crossmap
#' @param col_* character vector or values naming columns from `df`
#'
#' @returns An error if the validation condition fails,
#' and invisibly returns `df` otherwise.
#'
#' @name df_check
NULL

#' @describeIn df_check Abort if named columns can't be found in xmap_df
#'
df_check_cols <- function(df, col_attrs) {
  stopifnot(length(col_attrs) == 3)
  missing_cols <- setdiff(col_attrs, names(df))
  if (length(missing_cols) != 0) {
    cli::cli_abort(
      message = "The column{?s} {.var {missing_cols}} {?was/were} not found.",
      class = "abort_missing_cols"
    )
  }
  invisible(df)
}

#' @describeIn df_check Abort if xmap_df has missing values
#'
df_check_na <- function(df) {
  if (base::anyNA(df)) {
    cli::cli_abort(
      message = "NA values found. Please enter missing `from` or `to` node labels and/or convert NA weights",
      class = "abort_na"
    )
  }
  invisible(df)
}

#' @describeIn df_check Abort if xmap_df columns are not in order
df_check_col_order <- function(df, col_from, col_to, col_weights){
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

#' @describeIn df_check Abort if xmap_df has wrong column types
#'
df_check_col_type <- function(df, col_weights) {
  if (!is.numeric(df[[col_weights]])) {
    cli::cli_abort(
      message = "The column `{col_weights}` should be of type numeric",
      class = "abort_col_type"
    )
  }
  invisible(df)
}

#' @describeIn df_check Abort if from_set attribute doesn't match xmap_df values
#'
df_check_from_set <- function(df, col_from, from_set) {
  col_from_set <- as.character(unique(df[[col_from]]))
  stopifnot(identical(col_from_set, from_set))

  invisible(df)
}

#' @describeIn df_check Abort if xmap_df has duplicate links
#'
df_check_links <- function(df, col_from, col_to) {
  if (has_dup_links(df[[col_from]], df[[col_to]])) {
    cli::cli_abort(
      message = "Duplicate `from`-`to` links were found.
      Please remove or collapse duplicates.",
      class = "abort_dup"
    )
  }
  invisible(df)
}

#' @describeIn df_check Abort if df has invalid mapping weights
#'
df_check_weights <- function(df, col_from, col_weights) {
  if (!has_complete_weights(df[[col_from]], df[[col_weights]])) {
    cli::cli_abort(
      message = "Incomplete mapping weights found. Check sum of weights for each `from` group sums to 1",
      class = "abort_weights"
    )
  }
  invisible(df)
}
