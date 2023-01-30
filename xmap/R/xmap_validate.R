# Generated from create-xmap.Rmd: do not edit by hand

#' Validator for `xmap` objects
#'
#' @param x An `xmap` to be validated
#'
#' @return `x` if validation passes. Throws error otherwise.
#'
#' @section Methods:
#' \Sexpr[stage=render,results=rd]{generics:::methods_rd("xmap_validate")}
#'
#' @export
#'
xmap_validate <- function(x) {
  UseMethod("xmap_validate")
}

#' @export
xmap_validate.xmap_df <- function(x){
  validate_xmap_df(x)
}
