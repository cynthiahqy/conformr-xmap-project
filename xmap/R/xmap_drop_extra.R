# Generated from create-xmap.Rmd: do not edit by hand

#' Drop extra columns from `xmap` objects
#' 
#' @param .xmap an xmap object
#'
#' @export
xmap_drop_extra <- function(.xmap){
  stopifnot(is_xmap_df(.xmap))
  # get col names
  col_strings <- simplify2array(.get_col_attrs.xmap_df(.xmap))
  # construct new xmap with only necessary columns
  z_attrs <- attributes(.xmap)
  z_attrs$names <- unname(col_strings)
  z <- as.data.frame(.xmap)
  z <- z[,col_strings]
  attributes(z) <- z_attrs
  
  return(z)
}
