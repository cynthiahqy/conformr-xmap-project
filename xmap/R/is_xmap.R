# Generated from create-xmap.Rmd: do not edit by hand

#' Test if object is a crossmap
#'
#' This function returns `TRUE` for crossmaps `xmap` or subclasses thereof (`xmap_df`), and `FALSE` for all other objects, including regular data.frames or tibbles.
#' @export
#' @rdname as_xmap_df
is_xmap <- function(x) {
  inherits(x, "xmap")
}
