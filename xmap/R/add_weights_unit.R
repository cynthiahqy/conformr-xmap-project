# Generated from create-xmap.Rmd: do not edit by hand

#' Add unit weights to node pairs table
#'
#' Attaches column of unit weights to pairs of source-target nodes.
#' The resultant weighted links can be verified or coerced into `xmap`.
#'
#' @param pairs data.frame or tibble containing node pairs
#' @param weights_into character string naming new column to store link weights in
#'
#' @return `pairs` with additional column of ones
#' @export
#'
#' @examples
#' AUS_list <- list(AUS = c("NSW", "QLD", "SA", "TAS", "VIC", "WA", "ACT", "NT"))
#'   as_pairs_from_named(AUS_list, names_to = "ctr", values_to = "state") |>
#'   add_weights_unit(weights_into = "weights")
add_weights_unit <- function(pairs, weights_into = "weights"){
  pairs[,weights_into] <- 1
  return(pairs)
}
