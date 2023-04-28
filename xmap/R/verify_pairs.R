# Generated from create-xmap.Rmd: do not edit by hand

#' Verify crossmap properties of column pairs
#'
#' @param x a data frame-like object with at least two columns
#' @param from
#' @param to
#'
#' @return `x` or error
#' @name verify_pairs
NULL

#' @describeIn verify_pairs Verify column pairs have only one-to-one relations
#' @export
verify_pairs_all_1to1 <- function(x, from, to){
  stopifnot(is.data.frame(x))
  set_from <- unique(x[[rlang::englue("{{from}}")]])
  set_to <- unique(x[[rlang::englue("{{to}}")]])
  stopifnot(length(set_from) == length(set_to))
  invisible(x)
}

#' @describeIn verify_pairs Verify columnn pairs are all unique
#' @export
verify_pairs_all_unique <- function(x, from, to){
  stopifnot(is.data.frame(x))
  pairs <- dplyr::select(x, {{ from }}, {{to}})
  stopifnot(!as.logical(anyDuplicated(pairs)))
  invisible(x)
}

#' @describeIn verify_pairs (alias) Verify column pairs have only one-to-one relations
#' @export
verify_pairs_as_recode_unique <- verify_pairs_all_1to1

#' @describeIn verify_pairs Verify column pairs outgoing link degree
verify_pairs_out <- function(x, from, to, max_out, min_out){
  # TODO: FINISH THIS!
  dplyr::group_by(x, {{from}}) |>
  dplyr::summarise()
}
