# Generated from create-xmap.Rmd: do not edit by hand

#' Boolean flags for properties of candidate and validated xmap links
#'
#' @description
#' `has_*()` functions check properties of xmap links and/or candidate links.
#' The functions only accepts equal length vector inputs to support multiple link formats,
#' but does not check if the inputs are from the same xmap.
#' @param x_from,x_to,x_weights equal length vectors containing the source-target node pairs
#' 
#' @return TRUE or FALSE
#' 
#' @name has-links
NULL

#' @describeIn has-links Returns TRUE if xmap does not have 
#' duplicate pairs of source-target nodes (irrespective of weights)
#'
has_no_dup_pairs <- function(x_from, x_to) {
  stopifnot(is.vector(x_from))
  stopifnot(is.vector(x_to))
  stopifnot(identical(length(x_from), length(x_to)))
  links <- data.frame(x_from, x_to)
  dup_idx <- anyDuplicated(links)
  !as.logical(dup_idx)
}

#' @describeIn has-links Returns TRUE if all weights for a given `from` label
#' sum to one (approximately)
#' @param tol Tolerance of comparison. Passed through to the `tolerance` arg of
#' `base::all.equal()`.
has_complete_weights <- function(x_from, x_weights, tol = .Machine$double.eps^0.5) {
  stopifnot(is.vector(x_from))
  stopifnot(is.vector(x_weights))
  stopifnot(identical(length(x_from), length(x_weights)))
  sum_w <- tapply(
    X = x_weights,
    INDEX = x_from,
    FUN = sum,
    simplify = TRUE
  ) |> as.vector()
  names(sum_w) <- NULL
  ones <- rep(1, length(sum_w))
  all(isTRUE(all.equal(sum_w, ones, tolerance = tol)))
}

#' @describeIn has-links Return TRUE if xmap recodes labels between `from` and `to`
has_1to1 <- function(x_weights) {
  stopifnot(is.vector(x_weights))
  any(x_weights == 1)
}
#'
has_recode <- has_1to1

#' @describeIn has-links Return TRUE if xmap has splitting links between `from` and `to`
has_1toM <- function(x_weights) {
  stopifnot(is.vector(x_weights))
  any(x_weights < 1)
}
#'
has_split <- has_1toM

#' @describeIn has-links Return TRUE if xmap has collapsing links between `from` and `to`
has_1fromM <- function(x_to){
  stopifnot(is.vector(x_to))
  as.logical(anyDuplicated(x_to))
}
#'
has_collapse <- has_1fromM
