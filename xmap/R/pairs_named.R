# Generated from create-xmap.Rmd: do not edit by hand

#' Convert between column pairs and named vector or lists
#' 
#' @description
#' Convert named vectors or nested lists into a two-column table of node pairs and vice versa.
#' `as_pairs_from_named` extracts the vector or list element names and the values, unnesting where necessary.
#' `pairs_to_named_vector` extracts name-value pairs from column pairs as is,
#' whilst `pairs_to_named_list()` nests the values first.
#'
#' @inheritParams verify_named
#' @inheritParams verify_pairs
#' @param names_to,values_to character vector specify the new columns to pass the information in `x` into.
#' @param names_from,values_from two columns in `x` to convert to names and values
#'
#' @return
#'   * For `as_pairs_from_named()`: a two-column tibble
#'   * For `pairs_to_named` fncs: named vector or list 
#' 
#' @name as_pairs
#' @examples
#' # Coerce named vectors and list to column pairs
#' 
#' veg_vec <- c(eggplant = "aubergine", zucchini = "courgette")
#' as_pairs_from_named(veg_vec, "au_eng", "uk_eng")
#' 
#' animal_list <- list(MAMM = c("elephant", "whale", "monkey"),
#'                  REPT = c("lizard", "turtle"),
#'                  CRUS = c("crab"))
#' as_pairs_from_named(animal_list, "class", "animal")
#' 
#' # Convert pairs back to named vector and lists
#' veg_from_pairs <- as_pairs_from_named(veg_vec) |> 
#'   pairs_to_named_vector(names_from = name, values_from = value)
#' identical(veg_vec, veg_from_pairs)
#' 
#' animal_from_pairs <- as_pairs_from_named(animal_list, "class", "animal") |> 
#'   pairs_to_named_list(names_from = class, values_from = animal)
#' identical(animal_list, animal_from_pairs)
NULL

#' @describeIn as_pairs Convert named vector or nested list into column pairs
#' @export
as_pairs_from_named <- function(x, names_to = "name", values_to = "value"){
  stopifnot(is.vector(x))
  node_pairs <- x |>
    tibble::enframe(name = names_to, value = values_to) |>
    tidyr::unnest_longer(col=tidyr::all_of(values_to))
  return(node_pairs)
}

#' @describeIn as_pairs Convert column pairs to named vector
#' @export
pairs_to_named_vector <- function(df, names_from = name, values_from = value){
  ordered_cols <- dplyr::select(df, {{names_from}}, {{values_from}})
  tibble::deframe(ordered_cols)
}

#' @describeIn as_pairs Convert column pairs to nested named list
#' @export
pairs_to_named_list <- function(df, names_from = name, values_from = value){
  nested_cols <- dplyr::select(df, {{names_from}}, {{values_from}}) |>
    tidyr::nest(values = {{values_from}})
  ordered_cols <- dplyr::select(nested_cols, {{names_from}}, values)
  tibble::deframe(ordered_cols) |>
    sapply(as.matrix) |>
    sapply(as.vector) |>
    as.list()
}
