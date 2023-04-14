# Generated from create-xmap.Rmd: do not edit by hand

#' Coerce a unit weights `xmap_df` to a List
#'
#' @param x xmap with only unit weights
#'
#' @return named list. Each item contains a vector of source nodes which map to 
#' the target node named.
#' @export
#'
#' @examples
#' tar_list <- list(AA = c("x3", "x4", "x6"),
#'                  BB = c("x1", "x5"),
#'                  CC = c("x2"))
#' xmap_c <- links_from_list(tar_list, "source", "target", "weights") |>
#'            as_xmap_df(source, target, weights)
#' xmap_to_list(xmap_c)
xmap_to_list <- function(x) {
  x_attrs <- attributes(x)
  # check only unit weights
  w <- x[[x_attrs$col_weights]]
  stopifnot(all(w == 1))
  
  # convert
  x |>
    subset(select = c(x_attrs$col_to, x_attrs$col_from)) |>
    tidyr::nest(source = c(x_attrs$col_from)) |>
    tibble::deframe() |>
    sapply(as.matrix) |>
    sapply(as.vector)
}
