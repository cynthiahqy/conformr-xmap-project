# Generated from create-xmap.Rmd: do not edit by hand

#'
.calc_unique_sets.xmap_df <- function(x){
  stopifnot(is_xmap_df(x))
  df <- data.frame(x)
  x_attrs <- attributes(x)
  uniq_sets <- list()
  uniq_sets$from_set <- as.character(unique(df[[x_attrs$col_from]]))
  uniq_sets$to_set <- as.character(unique(df[[x_attrs$col_to]]))
  return(uniq_sets)
}
