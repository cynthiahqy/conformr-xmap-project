# Generated from create-xmap.Rmd: do not edit by hand

#' Add equal fractional weights to groups of source-target node pairs
#' 
#' Attaches equal weights to every source-target link from a given source node.
#' The resultant weighted links can be verified or coerced into an `xmap`.
#' 
#' @inheritParams verify_pairs
#' @inheritParams add_weights_unit
#' 
#' @return `pairs` with additional column of weights
#' @family {Helpers for adding weights to pairs}
#' @export
#' 
#' @examples
#' animal_pairs <- list(MAMM = c("elephant", "whale", "monkey"),
#'                  REPT = c("lizard", "turtle"),
#'                  CRUS = c("crab")) |>
#'   as_pairs_from_named("class", "animal")
#' animal_pairs |>
#'   add_weights_equal(from = class, to = animal)
add_weights_equal <- function(df, from, to, weights_into = "weights"){
  ## TODO: validate_pairs_unique()
  df |>
    dplyr::group_by({{from}}) |>
    dplyr::mutate("{weights_into}" := 1/dplyr::n_distinct({{to}})) |>
    dplyr::ungroup()
}
