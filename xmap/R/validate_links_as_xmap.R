# Generated from create-xmap.Rmd: do not edit by hand

#' Check if candidate links meet crossmap properties
#' 
#' @param df data.frame like object containing candidate links 
#' @inheritParams as_xmap_df
#' 
#' @export
#' @examples
#' tar_list <- list(AA = c("x3", "x4", "x6"),
#'                  BB = c("x1", "x5"),
#'                  CC = c("x2"))
validate_links_as_xmap <- function(df, from, to, weights){
  col_from <- deparse(substitute(from))
  col_to <- deparse(substitute(to))
  col_weights <- departse(substitute(weights))
  col_attrs <- c(col_from, col_to, col_weights)

  abort_missing_cols(df, col_attrs)
  abort_any_na(df)

  abort_dup_pairs(df, col_from, col_to)
  abort_bad_weights(df, col_from, col_weights)
  abort_weights_col_type(df, x_attrs$col_weights)

  invisible(df)
}
