# Generated from create-xmap.Rmd: do not edit by hand

#' Coerce a data.frame-like object to `xmap_df`
#'
#' This creates a valid crossmap which can be used to map numeric values `from` a set of source nodes `to` a set of target nodes.
#'
#' @param x
#'  * For `as_xmap_df()`: A data.frame or data.frame-like object
#'  * For `is_xmap()`: An object to test.
#' @param from,to Columns in `x` specifying the source and target nodes
#' @param weights Column in `x` specifying the weight applied to data passed along the directed link between source and target node
#' @param .keep_all Logical indicating whether or not to keep additional columns in `x`. Defaults to TRUE.
#'
#' @return A crossmap `xmap_df` S3 object.
#' @export
#'
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
#'
as_xmap_df <- function(x, from, to, weights, .keep_all = TRUE) {
  ## coercion & checks
  stopifnot(is.data.frame(x))

  # get string names for columns
  col_from <- deparse(substitute(from))
  col_to <- deparse(substitute(to))
  col_weights <- deparse(substitute(weights))
  col_strings <- c(col_from, col_to, col_weights)
  ## check columns exist
  df_check_cols(x, col_strings)

  ## drop additional columns
  if (!.keep_all) {
    df <- x[col_strings]
    if (ncol(df) < ncol(x)) {
    cli::cli_warn("Dropped additional columns in `x`")
      }
  } else {
    df <- x
  }

  ## rearrange columns
  col_order <- c(col_strings, setdiff(names(df), col_strings))
  df <- df[col_order]

  ## construction
  xmap <- new_xmap_df(df, col_from, col_to, col_weights)

  ## validation
  validate_xmap_df(xmap)

  return(xmap)
}
