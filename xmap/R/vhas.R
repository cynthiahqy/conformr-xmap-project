# Generated from create-xmap.Rmd: do not edit by hand

#' Boolean flags for properties of candidate and validated xmap links (internal)
#'
#' @description
#' `vhas_*()` functions check properties of xmap links and/or candidate links.
#' The functions only accepts equal length vector inputs to support multiple link formats,
#' but does not check if the inputs are from the same xmap.
#' @param v_from,v_to,v_weights equal length vectors containing the source-target node pairs
#' 
#' @return TRUE or FALSE
#' 
#' @name vhas
NULL

#' @describeIn vhas Returns TRUE if xmap does not have 
#' duplicate pairs of source-target nodes (irrespective of weights)
#'
vhas_no_dup_pairs <- function(v_from, v_to) {
  stopifnot(is.vector(v_from))
  stopifnot(is.vector(v_to))
  stopifnot(identical(length(v_from), length(v_to)))
  links <- data.frame(v_from, v_to)
  dup_idx <- anyDuplicated(links)
  !as.logical(dup_idx)
}

#' @describeIn vhas Returns TRUE if all weights for a given `from` label
#' sum to one (approximately)
#' @param tol numeric \eqn{\ge 0}. Ignore differences smaller than `tol`.
#' Passed through to the `tolerance` arg of `base::all.equal()`.
vhas_complete_weights <- function(v_from, v_weights, tol = .Machine$double.eps^0.5) {
  stopifnot(is.vector(v_from))
  stopifnot(is.vector(v_weights))
  stopifnot(identical(length(v_from), length(v_weights)))
  sum_w <- tapply(
    X = v_weights,
    INDEX = v_from,
    FUN = sum,
    simplify = TRUE
  ) |> as.vector()
  names(sum_w) <- NULL
  ones <- rep(1, length(sum_w))
  all(isTRUE(all.equal(sum_w, ones, tolerance = tol)))
}

.calc_vector_lens <- function(...){
  v_list <- list(...)
  v_lens <- sapply(v_list, length)
  return(v_lens)
}

#' @describeIn vhas Returns TRUE if links have no duplicate pairs and complete weights
vhas_xmap_props <- function(v_from, v_to, v_weights){
  ## check vectors are equal length
  v_lengths <- .calc_vector_lens(v_from, v_to, v_weights)
  stopifnot(length(unique(v_lengths)) == 1)
  
  ## check properties
  v_props <- c(
    pairs = vhas_no_dup_pairs(v_from, v_to),
    weights = vhas_complete_weights(v_from, v_weights)
  ) 
  all(v_props)
}

#' @describeIn vhas Return TRUE if xmap recodes labels between `from` and `to`
vhas_1to1 <- function(v_weights) {
  stopifnot(is.vector(v_weights))
  any(v_weights == 1)
}
#'
vhas_recode <- vhas_1to1

#' @describeIn vhas Return TRUE if xmap has splitting links between `from` and `to`
vhas_1toM <- function(v_weights) {
  stopifnot(is.vector(v_weights))
  any(v_weights < 1)
}
#'
vhas_split <- vhas_1toM

#' @describeIn vhas Return TRUE if xmap has collapsing links between `from` and `to`
vhas_1fromM <- function(v_to){
  stopifnot(is.vector(v_to))
  as.logical(anyDuplicated(v_to))
}
#'
vhas_collapse <- vhas_1fromM
