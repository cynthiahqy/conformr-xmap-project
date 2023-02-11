# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn as_xmap Coerce a `data.frame` to `xmap_df` or `xmap_tbl`
#'
#' @return A crossmap `xmap_df` S3 object.
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
as_xmap.data.frame <- function(x, from, to, weights, subclass = NULL, .keep_all = TRUE) {
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
  } else {
    df <- x
  }

  ## rearrange columns
  col_order <- c(col_strings, setdiff(names(df), col_strings))
  df <- df[col_order]

  ## construction
  subclass <- subclass %||% "xmap_df"
  xmap <- switch(subclass,
                 xmap_df = new_xmap_df(df, col_from, col_to, col_weights),
                 xmap_tbl = new_xmap_tbl(df, col_from, col_to, col_weights),
                 stop("Unknown xmap subclass"))

  ## validation
  validate_xmap_df(xmap)

  return(xmap)
}

# temp-fix to avoid rewriting tests
as_xmap_df <- as_xmap.data.frame


#' @describeIn as_xmap Coerce a `tibble` to `xmap_tbl` or `xmap_df`
#' 
#' @export
as_xmap.tbl_df <- function(x, from, to, weights, subclass = NULL, .keep_all = TRUE){
  stopifnot(tibble::is_tibble(x))

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
  } else {
    df <- x
  }

  ## rearrange columns
  col_order <- c(col_strings, setdiff(names(df), col_strings))
  df <- df[col_order]

  ## construction
  subclass <- subclass %||% "xmap_tbl"
  xmap <- switch(subclass,
                 xmap_df = new_xmap_df(df, col_from, col_to, col_weights),
                 xmap_tbl = new_xmap_tbl(df, col_from, col_to, col_weights),
                 stop("Unknown xmap subclass"))

  ## validation
  validate_xmap_df(xmap)

  return(xmap)
}
