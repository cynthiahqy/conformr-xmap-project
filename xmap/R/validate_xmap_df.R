# Generated from create-xmap.Rmd: do not edit by hand

#' Validator for `xmap_df` class (INTERNAL)
#'
#' Only checks class attributes, not crossmap graph properties.
#' Use `verify_links_as_xmap()` or `as_xmap()` to verify graph
#' properties
validate_xmap_df <- function(x) {
  stopifnot(is_xmap_df(x))

  df <- data.frame(x) # unclass(x)
  x_attrs <- attributes(x)
  col_attrs <- c(x_attrs$col_from, x_attrs$col_to, x_attrs$col_weights) 

  ## ---- df attributes ----
  abort_missing_cols(df, col_attrs)
  abort_any_na(df)
  
  ## ---- xmap_df attributes ---
  abort_col_order(df, x_attrs$col_from, x_attrs$col_to, x_attrs$col_weights)
  abort_from_set(df, x_attrs$col_from, x_attrs$from_set)

  ## return original object
  invisible(x)
}
