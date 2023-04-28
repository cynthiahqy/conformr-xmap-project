# Generated from create-xmap.Rmd: do not edit by hand

#' Drop extra columns from `xmap` objects
#'
#' @export
xmap_drop_extra <- function(x){
  UseMethod("xmap_drop_extra")
}

#' @export
xmap_drop_extra.xmap_df <- function(x){
  stopifnot(is_xmap_df(x))
  
  col_strings <- simplify2array(.get_col_attrs.xmap_df(x))
  
  z_attrs <- attributes(x)
  z_attrs$names <- unname(col_strings)
  
  z <- as.data.frame(x)
  z <- z[,col_strings]
  attributes(z) <- z_attrs
  
  return(z)
}
