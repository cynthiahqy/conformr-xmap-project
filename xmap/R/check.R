# Generated from create-xmap.Rmd: do not edit by hand

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
#' @name check-df
NULL

#' @describeIn check-df Abort if duplicate source-target pairs are found
#'
check_dup_pairs <- function(df, col_from, col_to) {
  if (!has_no_dup_pairs(df[[col_from]], df[[col_to]])) {
    cli::cli_abort(
      message = "Duplicate `from`-`to` links were found.
      Please remove or collapse duplicates.",
      class = "abort_dup_pairs"
    )
  }
  invisible(df)
}

#' @describeIn check-df Abort if invalid mapping weights are found
#'
check_bad_weights <- function(df, col_from, col_weights) {
  if (!has_complete_weights(df[[col_from]], df[[col_weights]])) {
    cli::cli_abort(
      message = "Incomplete mapping weights found. Check sum of weights for each `from` group sums to 1",
      class = "abort_bad_weights"
    )
  }
  invisible(df)
}
