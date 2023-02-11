# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn df_check Abort if xmap_df has wrong column types
#'
df_check_reversible <- function(df, col_to) {
  x_to <- df[[col_to]]
  if (has_collapse(x_to)){
    cli::cli_abort("Collapse links in {.var xmap_df} cannot be reversed. Please supply new weights and create a new xmap.")
  }
  invisible(df)
}
  
