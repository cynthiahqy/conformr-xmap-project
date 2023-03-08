#' Coerce objects to xmap
#' 
#' Validates and creates a valid crossmap `xmap` object of the same type as the input.
#' 
#' @param x
#'  * For `as_xmap()`: An object to coerce
#'  * For `is_xmap()`: An object to test.
#' @param from,to Columns in `x` specifying the source and target nodes
#' @param weights Column in `x` specifying the weight applied to data passed along the directed link between source and target node
#' @param subclass Which xmap subclass to return. Defaults to `xmap_df` for `data.frame`, and `xmap_tbl` for `tibble`
#' @param .keep_all Logical indicating whether or not to keep additional columns in `x`. Defaults to TRUE.
#' 
#' @return A validated `xmap` object.
#' 
#' @export
as_xmap <- function(x, from, to, weights, subclass = c("xmap_df", "xmap_tbl"), ...) {
  UseMethod("as_xmap")
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
#' as_xmap(links, from = a, to = b, weights = w)
#'
#' # extra columns are dropped,
#' links$extra <- c(2, 4, 5, 6)
#' as_xmap(links, from = a, to = b, weights = w)
as_xmap.data.frame <- function(x, from, to, weights, subclass = "xmap_df", .keep_all = TRUE) {
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
  if (.keep_all) {
    df <- x
  } else {
    df <- x[col_strings]
  }
  if (ncol(df) < ncol(x)) {
    cli::cli_inform("Dropped additional columns in `x`")
  }

  ## rearrange columns
  col_order <- c(col_strings, setdiff(names(df), col_strings))
  df <- df[col_order]

  ## construction
  xmap <- switch(subclass,
                 xmap_df = new_xmap_df(df, col_from, col_to, col_weights),
                 xmap_tbl = new_xmap_tbl(df, col_from, col_to, col_weights),
                 stop("Unknown xmap subclass"))

  ## validation
  validate_xmap_df(xmap)

  return(xmap)
}

#' @describeIn as_xmap Coerce `data.frame` or `tibble` to `xmap_df`
#' 
#' @export
as_xmap_df <- as_xmap.data.frame

#' @describeIn as_xmap Coerce a `tibble` to `xmap`
#' 
#' @export
as_xmap.tbl_df <- function(x, from, to, weights, subclass = "xmap_tbl", .keep_all = TRUE){
  stopifnot(tibble::is_tibble(x))

  # get string names for columns
  col_from <- deparse(substitute(from))
  col_to <- deparse(substitute(to))
  col_weights <- deparse(substitute(weights))
  col_strings <- c(col_from, col_to, col_weights)
  ## check columns exist
  df_check_cols(x, col_strings)

 ## drop additional columns
  if (.keep_all) {
    df <- x
  } else {
    df <- x[col_strings]
  }
  if (ncol(df) < ncol(x)) {
    cli::cli_inform("Dropped additional columns in `x`")
  }

  ## rearrange columns
  col_order <- c(col_strings, setdiff(names(df), col_strings))
  df <- df[col_order]

  ## construction
  xmap <- switch(subclass,
                 xmap_df = new_xmap_df(df, col_from, col_to, col_weights),
                 xmap_tbl = new_xmap_tbl(df, col_from, col_to, col_weights),
                 stop("Unknown xmap subclass"))

  ## validation
  validate_xmap_df(xmap)

  return(xmap)
}
