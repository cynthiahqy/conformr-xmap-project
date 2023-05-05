# Generated from create-xmap.Rmd: do not edit by hand

#' Add weights to unweighted links
#'
#' Attach column of weights to a table of unweighted source-target links.
#' If calculating equal fractional weights, uses distinct `from`-`to` pairs.
#' The resultant weighted links can be verified or coerced into `xmap`.
#'
#' @inheritParams verify_pairs
#' @param prop numeric column containing reference values to calculate fractional weights from.
#' Weights are calculated as `prop/sum(prop)` where the sum is calculated separately for each set of links coming out of a given `from` node.
#' @param weights_into character string naming new column to store link weights in
#'
#' @return `df` with additional column of weights
#' @export
#' @name add_weights
NULL

#' @describeIn add_weights Attach column of unit weights (i.e. ones)
#' @export
#' @examples
#' # simple unit weights
#' AUS_pairs <- list(AUS = c("NSW", "QLD", "SA", "TAS", "VIC", "WA", "ACT", "NT")) |>
#'   as_pairs_from_named(names_to = "ctr", values_to = "state")
#' AUS_pairs |>
#'   add_weights_unit(weights_into = "weights")
#'   
add_weights_unit <- function(df, weights_into = "weights"){
  df[,weights_into] <- 1
  return(df)
}

#' @describeIn add_weights Attach equal fractional weights by `from` group
#' @export
#' @examples
#' # fractional weights
#' animal_pairs <- list(MAMM = c("elephant", "whale", "monkey"),
#'                  REPT = c("lizard", "turtle"),
#'                  CRUS = c("crab")) |>
#'   as_pairs_from_named("class", "animal")
#' animal_pairs |>
#'   add_weights_equal(from = class, to = animal)
#'   
add_weights_equal <- function(df, from, to, weights_into = "weights"){
  xmap:::abort_dup_pairs(df, rlang::englue("{{from}}"), rlang::englue("{{to}}"))
  df |>
    dplyr::group_by({{from}}) |>
    dplyr::mutate("{weights_into}" := 1/dplyr::n_distinct({{to}})) |>
    dplyr::ungroup()
}

#' @describeIn add_weights Attach weights by `from` group using proportion reference `prop`
#' @export
#' @examples
#' # proportional weights
#' data.frame(recipe = c(rep("cake", 4), rep("pasta", 2)),
#'           ingredients = c(c("flour", "sugar", "eggs", "milk"), c("flour", "water")),
#'           grams = c(c(500, 250, 200, 250),c(250, 150))
#'           ) |>
#'  add_weights_prop(recipe, ingredients, grams)
add_weights_prop <- function(df, from, to, prop, weights_into = "weights"){
  xmap:::abort_dup_pairs(df, rlang::englue("{{from}}"), rlang::englue("{{to}}"))
  xmap:::abort_any_na(df)
  df |>
    dplyr::group_by({{from}}) |>
    dplyr::mutate("{weights_into}" := {{prop}}/sum({{prop}})) |>
    dplyr::ungroup()
}
