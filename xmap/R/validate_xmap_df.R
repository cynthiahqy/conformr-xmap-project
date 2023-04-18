# Generated from create-xmap.Rmd: do not edit by hand

#' Validator for `xmap_df` objects
#'
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

  ## ---- xmap graph properties ----
  abort_weights_col_type(df, x_attrs$col_weights)
  abort_dup_pairs(df, x_attrs$col_from, x_attrs$col_to)
  abort_bad_weights(df, x_attrs$col_from, x_attrs$col_weights)

  ## return original object
  invisible(x)
}
