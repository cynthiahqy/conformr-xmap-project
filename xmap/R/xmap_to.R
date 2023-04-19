# Generated from create-xmap.Rmd: do not edit by hand

#' Extract incidence matrix from xmap objects
#' 
#' Transforms `xmap` objects into incidence matrix where the rows are indexed by the `from` values
#' and the columns are indexed by `to` values. Drops any additional variables.
#' 
#' @param x an xmap object
#' @param sparse logical specifying if the result should be a sparse matrix. Defaults to TRUE.
#' @param ... Unused
#' 
#' @return A matrix or sparse matrix object
#' 
#' @export
xmap_to_matrix <- function(x, sparse, ...) {
  UseMethod("xmap_to_matrix")
}

#' @describeIn xmap_to_matrix Coerce a `xmap_df` to a Matrix
#'
#' @return
#' @export
#'
#' @examples
#' abc_xmap <- data.frame(
#'  stringsAsFactors = FALSE,
#'                  origin = c("a","b","c","d","e",
#'                           "f","g","h","i","i","j","j","j"),
#'                    dest = c("AA","AA","AA","AA",
#'                           "BB","BB","CC","DD","EE","FF","GG","HH","II"),
#'            link = c(1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5, 0.3, 0.3, 0.4)
#'  ) |>
#' as_xmap_df(origin, dest, link)
#' xmap_to_matrix(abc_xmap)
xmap_to_matrix.xmap_df <- function(x, sparse = TRUE){
  x_attrs <- attributes(x)
  fm <- paste(x_attrs$col_weights, "~", x_attrs$col_from, "+", x_attrs$col_to,
              collapse = "")
  x_df <- x |>
    as.data.frame(stringsAsFactors = TRUE)
  
  if(sparse){
    x_mtx <- stats::xtabs(as.formula(fm), x_df, sparse = TRUE)
  } else {
    x_mtx <- stats::xtabs(as.formula(fm), x_df, sparse = FALSE)
    attr(x_mtx, "call") <- NULL
    unclass(x_mtx)
  }
  return(x_mtx)
}

#' Coerce a unit weights `xmap_df` to a List
#'
#' @param x xmap with only unit weights (i.e. all weights should be 1)
#'
#' @return named list. Each item contains a vector of source nodes which map to 
#' the target node named.
#' @export
#'
#' @examples
xmap_to_list <- function(x) {
  x_attrs <- attributes(x)
  # check only unit weights
  w <- x[[x_attrs$col_weights]]
  if (!all(w == 1)) {
    cli::cli_abort("`x` must only have unit weights. Can't convert to list.",
                   class = "abort_weights_not_unit")
  }
  
  # convert
  x |>
    subset(select = c(x_attrs$col_to, x_attrs$col_from)) |>
    tidyr::nest(source = c(x_attrs$col_from)) |>
    tibble::deframe() |>
    sapply(as.matrix) |>
    sapply(as.vector)
}
