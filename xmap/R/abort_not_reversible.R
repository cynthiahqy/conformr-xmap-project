# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn abort Abort if xmap_df is not reversible without new weights
#'
abort_not_reversible <- function(df, col_to) {
  x_to <- df[[col_to]]
  if (has_collapse(x_to)){
    cli::cli_abort("Collapse links in {.var xmap_df} cannot be reversed. Please supply new weights and create a new xmap.")
  }
  invisible(df)
}
  
