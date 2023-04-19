# Generated from create-xmap.Rmd: do not edit by hand

#' Low Level Constructors for xmap subclasses
#' @param x data-frame object containing candidate links.
#' @param col_from,col_to,col_weights character strings naming columns containing source nodes, target nodes and numeric weights.
#' @return xmap_df object. Note that this function unclasses tibbles.
#' @name new_xmap
NULL

#' @describeIn new_xmap Construct xmap_df from data.frame
new_xmap_df <- function(x, col_from, col_to, col_weights, from_set = NULL) {
  #' checks argument types
  stopifnot(is.data.frame(x))
  stopifnot(length(col_from) == 1 && is.character(col_from))
  stopifnot(length(col_to) == 1 && is.character(col_to))
  stopifnot(length(col_weights) == 1 && is.character(col_weights))

  #' naively generates `from_set` if it is missing
  from_set <- from_set %||% as.character(unique(x[[col_from]]))
  stopifnot(is.vector(from_set, mode = "character"))

  #' @return `x` with additional subclasses `xmap_df` and `xmap`

  class(x) <- .calc_xmap_subclass_attr("xmap_df")
  structure(x,
    col_from = col_from,
    col_to = col_to,
    col_weights = col_weights,
    from_set = from_set
  )
}
