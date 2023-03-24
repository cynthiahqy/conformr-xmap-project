# Generated from create-xmap.Rmd: do not edit by hand

#' Extract incidence matrix from xmap objects
#' 
#' Transforms `xmap` objects into incidence matrix where the rows are indexed by the `from` values
#' and the columns are indexed by `to` values. Drops any additional variables.
#' 
#' @param x an xmap object
#' @param sparse logical specifying if the result should be a sparse matrix. Defaults to TRUE.
#' @param ... Unused
#' 
#' @return A matrix or sparse matrix object
#' 
#' @export
xmap_to_matrix <- function(x, sparse, ...) {
  UseMethod("xmap_to_matrix")
}
