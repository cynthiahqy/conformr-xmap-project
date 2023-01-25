# Generated from create-xmap.Rmd: do not edit by hand

#' Low Level Constructor for xmap_df
#'
new_xmap_df <- function(x = data.frame(), from, to, weights, from_set = NULL) {
  #' checks argument types
  stopifnot(is.data.frame(x))
  stopifnot(length(from) == 1 && is.character(from))
  stopifnot(length(to) == 1 && is.character(to))
  stopifnot(length(weights) == 1 && is.character(weights))

  #' naively generates `from_set` if it is missing
  from_set <- from_set %||% as.character(unique(x[[from]]))
  stopifnot(is.vector(from_set, mode = "character"))

  #' @return `x` with additional subclasses `xmap` and `xmap_df`

  class(x) <- append(c("xmap_df", "xmap"), class(x))
  structure(x,
    col_from = from,
    col_to = to,
    col_weights = weights,
    from_set = from_set
  )
}

new_xmap <- new_xmap_df
