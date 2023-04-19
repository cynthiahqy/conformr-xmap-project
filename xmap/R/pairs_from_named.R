# Generated from create-xmap.Rmd: do not edit by hand

#' Make candidate links from named list or vector
#' 
#' Convert a named list of one-to-one (named vector) or many-to-one (named list) relations 
#' into a table of node pairs. It extracts the vector or list element names into one set of nodes,
#' and the values into a second set of names, unnesting where necessary.
#'
#' @param x named list. Each item contains a vector of source nodes which map to #' the target node named.
#' @param names_to,values_to character vector specify the new columns to pass the information in `x` into.
#'
#' @return tibble containing .
#' @export
#'
#' @examples
#' tar_list <- list(MAMM = c("elephant", "whale", "monkey"),
#'                  REPT = c("lizard", "turtle"),
#'                  CRUS = c("crab"))
#' pairs_from_named(tar_list, "class", "animal")
#' 
#' tar_vec <- c(eggplant = "aubergine", zucchini = "courgette")
#' pairs_from_named(tar_vec, "au_eng", "uk_eng")
pairs_from_named <- function(x, names_to = "name", values_to = "value"){
  stopifnot(is.vector(x))
  node_pairs <- x |>
    tibble::enframe(name = names_to, value = values_to) |>
    tidyr::unnest_longer(col=tidyr::all_of(values_to))
  return(node_pairs)
}
