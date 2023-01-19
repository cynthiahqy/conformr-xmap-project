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
  x_attrs <- attributes(x)
  col_attrs <- simplify2array(x_attrs[startsWith(names(x_attrs), "col")])
  
  ## ---- df attributes ----
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

  ## col type validation
  if (!is.numeric(df[, x_attrs$col_weights])) {
    rlang::abort(
      message = "`col_weights` column in `x` should be numeric",
      class = "xmap_col_type"
    )
  }

  ## ---- xmap graph properties ----

  ## no missingness
  if (!has_no_NA(df)){
    cli::cli_abort(
      message = "NA values found. Please enter missing data and/or convert NA weights",
      class = "xmap_missing"
    )
  }

  ## no duplicate links
  if (!has_no_dup_links(df, x_attrs$col_from, x_attrs$col_to)){
    cli::cli_abort(
      message = "Duplicate `from`-`to` links were found. Please remove or collapse duplicates.",
      class = "xmap_dup"
    )
  }

  ## complete weight check
  if(!has_complete_weights(df, x_attrs$col_from, x_attrs$col_weights)){
    cli::cli_abort(
      message = "Incomplete mapping weights found. Check sum of weights for each `from` group sums to 1",
      class = "xmap_weights"
    )
  }
}
