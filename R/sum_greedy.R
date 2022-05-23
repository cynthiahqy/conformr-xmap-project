#' NA-preserving Sum of Vector Elements
#'
#' Logical switching of \code{na.rm} argument for \code{\link[base]{sum}}
#' based on whether all inputs are \code{NA}.
#'
#' Preserves returning of \code{NA} if all elements are \code{NA}.
#' Otherwise, behaves like \code{sum(x, na.rm=TRUE)} and
#' returns sum of non-missing elements.
#'
#' @param x vector
#' @export
#'
#' @return `NA` if all elements in `x` are `NA`.
#' Otherwise, sum of all non-missing elements in vector.
#'
#' @examples
#' sum_NA(1:4)          ## returns 10
#' sum_NA(c(NA,NA,NA))  ## returns NA
#' sum_NA(c(2, NA, 32)) ## returns 34
#'
sum_greedy <- function(x){
  switch <- !all(is.na(x))
  sum(x, na.rm = switch)
}

