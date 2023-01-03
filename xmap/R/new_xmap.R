# Generated from create-xmap.Rmd: do not edit by hand

#' Low Level Constructor for xmap
new_xmap <- function(x = data.frame(), from, to, weights) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(from))
  stopifnot(is.character(to))
  stopifnot(is.character(weights))
  
  class(x) <- append("xmap_df", class(x))

  structure(x, 
            src_node = from,
            tar_node = to,
            link_weight = weights)
}
