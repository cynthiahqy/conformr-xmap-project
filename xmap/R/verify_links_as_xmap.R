# Generated from create-xmap.Rmd: do not edit by hand

#' Check if candidate links meet crossmap properties
#'
#' The links must satisfy:
#'  - no missing values
#'  - at most one link between each unique source and target node
#'  - the weights on links coming out of each source node sum to 1
#'
#' @param df data.frame like object containing candidate links
#' @inheritParams vhas_complete_weights
#' @inheritParams as_xmap
#'
#' @export
#' @examples
#' # For a well formed crossmap:
#' links <- data.frame(
#'   a = "AUS",
#'   b = c("VIC", "NSW", "WA", "OTHER"),
#'   w = c(0.1, 0.15, 0.25, 0.5)
#' )
#' verify_links_as_xmap(links, from = a, to = b, weights = w)
verify_links_as_xmap <- function(df, from, to, weights, tol = .Machine$double.eps^0.5) {
  col_from <- deparse(substitute(from))
  col_to <- deparse(substitute(to))
  col_weights <- deparse(substitute(weights))
  col_attrs <- c(col_from, col_to, col_weights)
  abort_missing_cols(df, col_attrs)
  abort_any_na(df)
  abort_weights_col_type(df, col_weights)
  abort_dup_pairs(df, col_from, col_to)
  stop_bad_weights <- !vhas_complete_weights(df[[col_from]], df[[col_weights]], tol)
  if (stop_bad_weights) {
    abort_bad_weights(col_weights)
  }

  invisible(df)
}
