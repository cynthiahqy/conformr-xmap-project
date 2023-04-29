# Generated from create-xmap.Rmd: do not edit by hand

#' Verify crossmap properties of column pairs
#'
#' @param df a data frame-like object with at least two columns
#' @inheritParams verify_links_as_xmap
#'
#' @return `df` or error
#' @name verify_pairs
NULL

#' @describeIn verify_pairs Verify column pairs have only one-to-one relations
#' @export
verify_pairs_all_1to1 <- function(df, from, to){
  stopifnot(is.data.frame(df))
  set_from <- unique(df[[rlang::englue("{{from}}")]])
  set_to <- unique(df[[rlang::englue("{{to}}")]])
  stopifnot(length(set_from) == length(set_to))
  invisible(df)
}

#' @describeIn verify_pairs Verify column pairs are all unique
#' @export
verify_pairs_all_unique <- function(df, from, to){
  stopifnot(is.data.frame(df))
  pairs <- dplyr::select(df, {{ from }}, {{to}})
  stopifnot(!as.logical(anyDuplicated(pairs)))
  invisible(df)
}

#' @describeIn verify_pairs Alias of `verify_pairs_all_1to1`
#' @export
verify_pairs_as_recode_unique <- verify_pairs_all_1to1

# @describeIn verify_pairs Verify column pairs outgoing link degree
verify_pairs_out <- function(x, from, to, max_out, min_out){
  # TODO: FINISH THIS!
  dplyr::group_by(x, {{from}}) |>
  dplyr::summarise()
}
