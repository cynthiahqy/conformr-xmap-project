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

#' Validation messages for xmap or candidate links
#'
#' @description
#' Checks issues with data.frame like objects containing validated `xmap` or candidate links.
#'
#' @param df a data.frame-like object containing links
#' @param col_from, col_to, col_weights character vector or values naming columns from `df`
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

#' @describeIn abort Abort if invalid mapping weights are found
#'
abort_bad_weights <- function(df, col_from, col_weights) {
  if (!vhas_complete_weights(df[[col_from]], df[[col_weights]])) {
    cli::cli_abort(
      message = "Incomplete mapping weights found. Check sum of weights for each `from` group sums to 1",
      class = "abort_bad_weights"
    )
  }
  invisible(df)
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

#' @describeIn abort Abort if xmap_df is not reversible without new weights
#'
abort_not_reversible <- function(df, col_to) {
  x_to <- df[[col_to]]
  if (vhas_collapse(x_to)){
    cli::cli_abort("Collapse links in {.var xmap_df} cannot be reversed. Please supply new weights and create a new xmap.")
  }
  invisible(df)
}
  
