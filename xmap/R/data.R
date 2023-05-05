# Generated from create-xmap.Rmd: do not edit by hand

#' Mock input objects for the `xmap` package
#' 
#' A collection of mock inputs for experimenting with functions
#' in the `xmap` package.
#' `named_` objects are either named vectors or nested lists.
#' `df_` objects may contain source-target *pairs* (no weights),
#' or weighted source-target *links*.
#' 
#' @format ## `mock`
#' A list with:
#' \describe{
#'  \item{named_ctr_iso3c}{named vector with 249 elements. Names are ISO-3 country codes, values are ISO English country names. Retrieved from `countrycode` package:
#'    \url{https://github.com/vincentarelbundock/countrycode}}
#'  \item{df_anzsco21}{tibble with 51 rows and 4 columns. Contains major and submajor occupation codes and descriptions for ANZSCO21. Retrieved from `strayr` package:
#'    \url{https://github.com/runapp-aus/strayr}}
#'  \item{xmap_abc}{xmap_df: lower -> upper BY share. Mock crossmap with 6 links including one-to-one, one-to-many and many-to-one relations.}
#'  \item{named_aus}{named list with 1 element named "AUS" containing codes for the Australian states}
#'  \item{df_aus_pop}{tibble containing 2022 population figures for Australia by state. Retrieved from:
#'    \url{https://www.abs.gov.au/statistics/people/population/national-state-and-territory-population/jun-2022}}
#'  }
#'@examples
#' links_aus_agg <- mock$named_aus |>
#'   as_pairs_from_named(names_to = "ctr", values_to = "state") |>
#'   add_weights_unit() |>
#'   dplyr::select(ctr, state, weights) |>
#'   verify_links_as_xmap(from = state, to = ctr, weights)
#' links_aus_agg
#'
#' links_aus_split_equal <- links_aus_agg |>
#'   add_weights_equal(from = ctr, to = state) |>
#'   dplyr::select(ctr, state, weights) |>
#'   verify_links_as_xmap(from = ctr, to = state, weights)
#' links_aus_split_equal
#'
#' links_aus_split_pop <- mock$df_aus_pop |>
#'   add_weights_prop(from = ctr, to = state, prop = pop)
#' links_aus_split_pop
"mock"
