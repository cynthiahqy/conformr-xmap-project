# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn xmap_to_matrix Coerce a `data.frame` to `xmap`
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
