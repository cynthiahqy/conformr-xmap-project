# Generated from create-xmap.Rmd: do not edit by hand

#' Make candidate links from list
#' 
#' Convert a named list of one-to-one or many-to-one relations into a table of
#' candidate links for validation or coercion into a crossmap \(i.e. via `as_xmap_df()`\).
#'
#' @param x_list named list. Each item contains a vector of source nodes which map to #' the target node named.
#' @param col_from,col_to,col_weights character string names for columns containing
#' source nodes, target nodes and numeric weights.
#'
#' @return tibble containing candidate links for conversion to xmap.
#' @export
#'
#' @examples
#' tar_list <- list(AA = c("x3", "x4", "x6"),
#'                  BB = c("x1", "x5"),
#'                  CC = c("x2"))
#' links_from_list(tar_list, "source", "target", "weights")
links_from_list <- function(x_list, col_from, col_to, col_weights){
  stopifnot(is.vector(x_list))
  links <- x_list |>
    tibble::enframe(name = col_to, value = col_from) |>
    tidyr::unnest_longer(col=tidyr::all_of(col_from))
    #dplyr::mutate("{col_weights}" := 1)
  links[[col_weights]] <- 1
  return(links)
}
