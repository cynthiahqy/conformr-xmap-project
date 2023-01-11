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
  df <- data.frame(x) # unclass(x)
  xmap_attrs <- attributes(x)
  col_attrs <- simplify2array(xmap_attrs[startsWith(names(xmap_attrs), "col")])
  
  ## dimension check
  if (ncol(df) != 3) {
    rlang::abort(
      message = "`x` must only have the three columns: from, to, weights",
      class = "xmap_ncol"
    )
  }

  ## cols present
  missing_cols <- setdiff(names(df), col_attrs)
  if (length(missing_cols) != 0){
    cli::cli_abort(
      message = "The column{?s} `{missing_cols}` {?was/were} not found in `x`.",
      class = "xmap_col_not_found"
    )
  }

  ## type validation
  if (!is.numeric(df[, xmap_attrs$col_weights])) {
    rlang::abort(
      message = "`col_weights` column in `x` should be numeric",
      class = "xmap_col_type"
    )
  }

  ## from_set validation
  # stopifnot(is.vector(from_set, mode = "character"))

  ## duplication and missingness checks

  ## complete weight check
}
