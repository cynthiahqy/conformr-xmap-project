# Generated from create-xmap.Rmd: do not edit by hand

#'
.get_col_attrs.xmap_df <- function(x){
  stopifnot(is_xmap_df(x))
  x_attrs <- attributes(x)
  col_attrs <- x_attrs[startsWith(names(x_attrs), "col")]
  return(col_attrs)
}
