# Generated from create-xmap.Rmd: do not edit by hand

#' Coerce objects to xmap_df
#' 
#' Validates and creates a valid crossmap `xmap_df` object.
#' 
#' @param x
#'  * For `as_xmap_df()`: An object to coerce
#'  * For `is_xmap_df()`: An object to test.
#' @param from,to Columns in `x` specifying the source and target nodes
#' @param weights Column in `x` specifying the weight applied to data passed along the directed link between source and target node
#' @inheritParams vhas_complete_weights
#' @param subclass Which xmap subclass to return. Defaults to `xmap_df` for `data.frame` and `tibble`
#' @param .drop_extra Drop columns other than `from`, `to` and `weights`. Defaults to `TRUE`
#' 
#' @return A validated `xmap` object.
#' @name as_xmap
#' @export
as_xmap_df <- function(x, from, to, weights, tol = .Machine$double.eps^0.5, subclass = c("xmap_df"), ...) {
  UseMethod("as_xmap_df")
}

#' @describeIn as_xmap Coerce a `data.frame` to `xmap`
#'
#' @export
#' @examples
#' # For a well formed crossmap:
#' links <- data.frame(
#'   a = "AUS",
#'   b = c("VIC", "NSW", "WA", "OTHER"),
#'   w = c(0.1, 0.15, 0.25, 0.5)
#' )
#' as_xmap_df(links, from = a, to = b, weights = w)
#'
#' # extra columns are dropped,
#' links$extra <- c(2, 4, 5, 6)
#' as_xmap_df(links, from = a, to = b, weights = w)
as_xmap_df.data.frame <- function(x, from, to, weights, tol = .Machine$double.eps^0.5, subclass = "xmap_df", .drop_extra = TRUE) {
  ## coercion & checks
  stopifnot(is.data.frame(x))

  # get string names for columns
  col_from <- deparse(substitute(from))
  col_to <- deparse(substitute(to))
  col_weights <- deparse(substitute(weights))
  col_strings <- c(col_from, col_to, col_weights)
  ## check columns exist
  abort_missing_cols(x, col_strings)

  ## drop additional columns
  if (.drop_extra) {
    df <- x[col_strings]
  } else {
    df <- x
  }
  if (ncol(df) < ncol(x)) {
    cli::cli_inform("Dropped additional columns in {.arg {deparse(substitute(x))}}")
  }

  ## rearrange columns
  col_order <- c(col_strings, setdiff(names(df), col_strings))
  df <- df[col_order]

  ## construction
  xmap <- switch(subclass,
                 xmap_df = new_xmap_df(df, col_from, col_to, col_weights),
                 stop("Unknown xmap subclass"))

  ## validation
  ## ---- xmap graph properties ----
  abort_weights_col_type(df, col_weights)
  abort_dup_pairs(df, col_from, col_to)
  stop_bad_weights <- !vhas_complete_weights(df[[col_from]], df[[col_weights]], tol)
  if (stop_bad_weights) { abort_bad_weights(col_weights) }

  ## ---- xmap_df attributes ----
  validate_xmap_df(xmap)

  return(xmap)
}
