# Generated from create-xmap.Rmd: do not edit by hand

#' Make candidate node pairs from named list or vector
#' 
#' Convert a named list of one-to-one (named vector) or many-to-one (named list) relations 
#' into a two-column table of node pairs.
#' It extracts the vector or list element names and the values, unnesting where necessary.
#'
#' @param x named list or vector. Each item contains a vector or value of source node(s) which map to
#' the target node named.
#' @param names_to,values_to character vector specify the new columns to pass the information in `x` into.
#'
#' @return tibble containing .
#' @export
#'
#' @examples
#' veg_vec <- c(eggplant = "aubergine", zucchini = "courgette")
#' as_pairs_from_named(veg_vec, "au_eng", "uk_eng")
#' 
#' animal_list <- list(MAMM = c("elephant", "whale", "monkey"),
#'                  REPT = c("lizard", "turtle"),
#'                  CRUS = c("crab"))
#' as_pairs_from_named(animal_list, "class", "animal")
as_pairs_from_named <- function(x, names_to = "name", values_to = "value"){
  stopifnot(is.vector(x))
  node_pairs <- x |>
    tibble::enframe(name = names_to, value = values_to) |>
    tidyr::unnest_longer(col=tidyr::all_of(values_to))
  return(node_pairs)
}
