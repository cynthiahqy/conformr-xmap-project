# Generated from create-xmap.Rmd: do not edit by hand

#' Test if object is a crossmap
#'
#' This function returns `TRUE` for crossmaps `xmap` or subclasses thereof (`xmap_df`), and `FALSE` for all other objects, including regular data.frames or tibbles.
#' @export
#' @rdname as_xmap
is_xmap <- function(x) {
  base::inherits(x, "xmap")
}

#' Test if object is `xmap_df`
#' @export
#' @rdname as_xmap
is_xmap_df <- function(x) {
  rlang::inherits_all(x, c("xmap_df", "xmap"))
}
