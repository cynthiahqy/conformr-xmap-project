# Generated from create-xmap.Rmd: do not edit by hand

#' A Validator for `xmap` objects
#'
#' @param x An `xmap` to be validated
#'
#' @return `x` if validation passes. Throws error otherwise.
#' @export
#'
#' @examples
validate_xmap <- function(x){
  xmap <- data.frame(x) # unclass(x)
  src_col <- attr(x, "src_node")
  tar_col <- attr(x, "tar_node")
  weight_col <- attr(x, "link_weight")
  col_attrs <- c(src_col, tar_col, weight_col)
  
  ## dimension check
  if (ncol(xmap) != 3) {
    rlang::abort(
      message = "`x` must only have the three columns: from, to, weights",
      class = "xmap_ncol"
    )
  }

  ## cols present
  if (length(setdiff(names(xmap), col_attrs)) != 0){
    rlang::abort(
      message = "`src_node`, `tar_node` or `link_weight` column not found in `x`.",
      class = "xmap_col_not_found"
    )
  }

  ## type validation

  ## duplication and missingness checks
  
  ## complete weight check
}
