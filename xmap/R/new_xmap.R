# Generated from create-xmap.Rmd: do not edit by hand

#' Low Level Constructor for xmap
new_xmap <- function(x = data.frame(), from, to, weights) {
  stopifnot(is.data.frame(x))
  stopifnot(is.character(from))
  stopifnot(is.character(to))
  stopifnot(is.character(weights))
  
  structure(x, 
            class = "crossmap_xmap",
            src_nodes = from,
            tar_nodes = to,
            link_weights = weights)
}
