# Generated from create-xmap.Rmd: do not edit by hand

#' Validator for xmap_df objects
#'
#' @rdname validate_xmap
validate_xmap_df <- function(x) {
  stopifnot(inherits(x, "xmap_df"))

  df <- data.frame(x) # unclass(x)
  x_attrs <- attributes(x)
  col_attrs <- simplify2array(x_attrs[startsWith(names(x_attrs), "col")])

  ## ---- df attributes ----
  df_check_cols(df, col_attrs)
  df_check_na(df)
  df_check_col_type(df, x_attrs$col_weights)
  df_check_from_set(df, x_attrs$col_from, x_attrs$from_set)


  ## ---- xmap graph properties ----
  df_check_links(df, x_attrs$col_from, x_attrs$col_to)
  df_check_weights(df, x_attrs$col_from, x_attrs$col_weights)

  ## return original object
  invisible(x)
}

#' Validator for `xmap` objects
#'
#' @param x An `xmap` to be validated
#'
#' @return `x` if validation passes. Throws error otherwise.
#' @export
#' @name validate_xmap
#'
validate_xmap <- validate_xmap_df

xmap_validate <- validate_xmap
