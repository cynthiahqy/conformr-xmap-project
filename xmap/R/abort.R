# Generated from create-xmap.Rmd: do not edit by hand

#' Abort if named columns can't be found in df
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

#' Abort if xmap_df has missing values
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
