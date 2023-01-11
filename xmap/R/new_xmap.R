# Generated from create-xmap.Rmd: do not edit by hand

#' Low Level Constructor for xmap
new_xmap <- function(x = data.frame(), from, to, weights) {
  stopifnot(is.data.frame(x))
  stopifnot(length(from) == 1 && is.character(from))
  stopifnot(length(to) == 1 && is.character(to))
  stopifnot(length(weights) == 1 && is.character(weights))
  
  class(x) <- append("xmap_df", class(x))

  from_set <- unique(x[[from]])

  structure(x, 
            col_from = from,
            col_to = to,
            col_weights = weights,
            from_set = from_set)
}
