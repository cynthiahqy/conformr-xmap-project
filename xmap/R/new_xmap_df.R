# Generated from create-xmap.Rmd: do not edit by hand

#' Low Level Constructor for xmap_df
#'
#' @param col_from,col_to,col_weights character strings naming columns containing source nodes, target nodes and numeric weights.
new_xmap_df <- function(x = data.frame(), col_from, col_to, col_weights, from_set = NULL) {
  #' checks argument types
  stopifnot(is.data.frame(x))
  stopifnot(length(col_from) == 1 && is.character(col_from))
  stopifnot(length(col_to) == 1 && is.character(col_to))
  stopifnot(length(col_weights) == 1 && is.character(col_weights))

  #' naively generates `from_set` if it is missing
  from_set <- from_set %||% as.character(unique(x[[col_from]]))
  stopifnot(is.vector(from_set, mode = "character"))

  #' @return `x` with additional subclasses `xmap` and `xmap_df`

  class(x) <- append(c("xmap_df", "xmap"), class(x))
  structure(x,
    col_from = col_from,
    col_to = col_to,
    col_weights = col_weights,
    from_set = from_set
  )
}
