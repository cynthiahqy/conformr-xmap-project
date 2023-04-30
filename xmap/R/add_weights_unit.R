# Generated from create-xmap.Rmd: do not edit by hand

#' Add unit weights to node pairs table
#'
#' Attaches column of unit weights to pairs of source-target nodes.
#' The resultant weighted links can be verified or coerced into `xmap`.
#'
#' @inheritParams verify_pairs
#' @param weights_into character string naming new column to store link weights in
#'
#' @return `pairs` with additional column of ones
#' @family {Helpers for adding weights to pairs}
#' @export
#'
#' @examples
#' AUS_pairs <- list(AUS = c("NSW", "QLD", "SA", "TAS", "VIC", "WA", "ACT", "NT")) |>
#'   as_pairs_from_named(names_to = "ctr", values_to = "state")
#' AUS_pairs |>
#'   add_weights_unit(weights_into = "weights")
add_weights_unit <- function(df, weights_into = "weights"){
  ## TODO: validate_pairs_unique()
  df[,weights_into] <- 1
  return(df)
}
