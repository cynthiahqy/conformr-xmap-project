#' Boolean flags for xmap properties
#'
#' @description
#' `has_*()` and `is_*()` functions check properties used to validate and/or print xmaps.
#' The functions only accepts equal length vector inputs to support all subclasses of `xmap`,
#' but does not check if the inputs are from the same xmap.
#' @param x_from,x_to,x_weights equal length vectors containing the source-target node pairs
#' @name xmap-has
NULL

#' @describeIn xmap-has Returns TRUE if xmap has duplicate links
#'
has_dup_links <- function(x_from, x_to) {
  stopifnot(is.vector(x_from))
  stopifnot(is.vector(x_to))
  stopifnot(identical(length(x_from), length(x_to)))
  links <- data.frame(x_from, x_to)
  dup_idx <- anyDuplicated(links)
  as.logical(dup_idx)
}

#' @describeIn xmap-has Returns TRUE if xmap has valid weights
has_complete_weights <- function(x_from, x_weights) {
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
  all(isTRUE(all.equal(sum_w, ones)))
}

#' @describeIn xmap-has Return TRUE if xmap recodes labels between `from` and `to`
has_1to1 <- function(x_weights) {
  stopifnot(is.vector(x_weights))
  any(x_weights == 1)
}
#'
has_recode <- has_1to1

#' @describeIn xmap-has Return TRUE if xmap has splitting links between `from` and `to`
has_1toM <- function(x_weights) {
  stopifnot(is.vector(x_weights))
  any(x_weights < 1)
}
#'
has_split <- has_1toM

#' @describeIn xmap-has Return TRUE if xmap has collapsing links between `from` and `to`
has_1fromM <- function(x_to){
  stopifnot(is.vector(x_to))
  as.logical(anyDuplicated(x_to))
}
#'
has_collapse <- has_1fromM

#' @describeIn xmap-has Return TRUE if xmap ONLY recodes labels between `from` and `to`. Return FALSE if there is even one collapsing or splitting link
#'
has_recode_only <- function(x_weights, x_to){
  stopifnot(identical(length(x_weights), length(x_to)))
  has_recode(x_weights) && !has_split(x_weights) && !has_collapse(x_to)
}

#' @describeIn xmap-has Return TRUE if xmap has at least one set of collapsing links, any number of recoding links, and no splitting links.
has_collapse_recode_only <- function(x_weights, x_to){
  stopifnot(identical(length(x_weights), length(x_to)))
  has_collapse(x_to) && !has_split(x_weights)
}

#' @describeIn xmap-has Return TRUE if xmap has at least one set of splitting links, any number of recoding links (or none), and no splitting
has_split_recode_only <- function(x_weights, x_to){
  stopifnot(identical(length(x_weights), length(x_to)))
  has_split(x_weights) && !has_collapse(x_to)
}
