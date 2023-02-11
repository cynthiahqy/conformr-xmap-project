# Generated from create-xmap.Rmd: do not edit by hand

#' Coerce objects to xmap
#' 
#' Validates and creates a valid crossmap `xmap` object of the same type as the input.
#' 
#' @param x
#'  * For `as_xmap()`: An object to coerce
#'  * For `is_xmap()`: An object to test.
#' @param from,to Columns in `x` specifying the source and target nodes
#' @param weights Column in `x` specifying the weight applied to data passed along the directed link between source and target node
#' @param subclass Which xmap subclass `df` or `tbl` to return. Defaults to `xmap_df` for `data.frame`, and `xmap_tbl` for `tibble`
#' @param .keep_all Logical indicating whether or not to keep additional columns in `x`. Defaults to TRUE.
#' 
#' @return A validated `xmap` object.
#' 
#' @export
as_xmap <- function(x, from, to, weights, subclass, ...) {
  UseMethod("as_xmap")
}

