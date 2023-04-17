# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn check-df Abort if crossmap properities are not met
check_links_as_xmap <- function(df, from, to, weights){
  col_from <- deparse(substitute(from))
  col_to <- deparse(substitute(to))
  col_weights <- departse(substitute(weights))
  col_attrs <- c(col_from, col_to, col_weights)
  
  abort_missing_cols(links_df, col_attrs)
  abort_any_na(links_df)
  
  check_dup_pairs(df, col_from, col_to)
  check_bad_weights(df, col_from, col_weights)
  
  invisible(df)
}
