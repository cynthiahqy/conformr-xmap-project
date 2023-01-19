# Generated from create-xmap.Rmd: do not edit by hand

#' df has complete weights
#'
has_complete_weights <- function(df, from, weights){
  sum_w <- tapply(df[[weights]], df[[from]], sum, simplify = TRUE)

  all(sum_w == 1)
}
