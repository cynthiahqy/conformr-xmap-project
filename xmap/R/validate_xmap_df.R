# Generated from create-xmap.Rmd: do not edit by hand

#' Validator for `xmap_df` objects
#'
validate_xmap_df <- function(x) {
  stopifnot(is_xmap_df(x))

  df <- data.frame(x) # unclass(x)
  x_attrs <- attributes(x)
  col_strings <- simplify2array(.get_col_attrs.xmap_df(x))

  ## ---- df attributes ----
  df_check_cols(df, col_strings)
  df_check_col_order(df, x_attrs$col_from, x_attrs$col_to, x_attrs$col_weights)
  df_check_na(df)
  df_check_col_type(df, x_attrs$col_weights)
  df_check_from_set(df, x_attrs$col_from, x_attrs$from_set)


  ## ---- xmap graph properties ----
  df_check_links(df, x_attrs$col_from, x_attrs$col_to)
  df_check_weights(df, x_attrs$col_from, x_attrs$col_weights)

  ## return original object
  invisible(x)
}
