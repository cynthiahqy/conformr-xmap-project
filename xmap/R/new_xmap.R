# Generated from create-xmap.Rmd: do not edit by hand

#' Low Level Constructor for xmap
new_xmap <- function(x = data.frame(), from, to, weights) {
  ## argument type checks
  stopifnot(is.data.frame(x))
  stopifnot(ncol(x) == 3) ## validate
  stopifnot(length(from) == 1 && is.character(from))
  stopifnot(length(to) == 1 && is.character(to))
  stopifnot(length(weights) == 1 && is.character(weights))

  ## column type checks
  from_set <- unique(x[[from]])
  stopifnot(is.vector(from_set, mode = "character"))   ## validate 
  stopifnot(is.vector(x[[weights]], mode = "numeric")) ## validate

  ## class constructor
  class(x) <- append("xmap_df", class(x))
  structure(x, 
            col_from = from,
            col_to = to,
            col_weights = weights,
            from_set = from_set)
}
