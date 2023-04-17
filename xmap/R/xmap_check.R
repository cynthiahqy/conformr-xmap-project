# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn check Abort if xmap_df columns are not in order
check_col_order <- function(df, col_from, col_to, col_weights){
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

#' @describeIn check Abort if xmap_df has wrong column types
#'
check_weights_col_type <- function(df, col_weights) {
  if (!is.numeric(df[[col_weights]])) {
    cli::cli_abort(
      message = "The column `{col_weights}` should be of type numeric",
      class = "abort_col_type"
    )
  }
  invisible(df)
}

#' @describeIn validate_xmap Abort if from_set attribute doesn't match xmap_df values
#'
check_from_set <- function(df, col_from, from_set) {
  col_from_set <- as.character(unique(df[[col_from]]))
  stopifnot(identical(col_from_set, from_set))

  invisible(df)
}
