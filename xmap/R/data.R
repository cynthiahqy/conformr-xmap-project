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
#'  \item{named_ctr_iso3c}{named vector. Names are ISO-3 country codes, values are ISO English country names. Retrieved from `countrycode` package:
#'    \url{https://github.com/vincentarelbundock/countrycode}}
#'  \item{df_anzsco21}{4-column tibble. Contains major and submajor occupation codes and descriptions for ANZSCO21. Retrieved from `strayr` package:
#'    \url{https://github.com/runapp-aus/strayr}}
#'  \item{df_mixed}{3-column data.frame. }
#'  }
"mock"
