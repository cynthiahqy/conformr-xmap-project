# Generated from create-xmap.Rmd: do not edit by hand

#' @describeIn has-links Returns TRUE if links have no duplicate pairs and complete weights
has_xmap_props <- function(x_from, x_to, x_weights){
  ## check vectors are equal length
  x_list <- list(x_from, x_to, x_weights)
  x_lengths <- sapply(x_list, length)
  stopifnot(length(unique(x_lengths)) == 1)
  
  ## check properties
  x_props <- c(
    pairs = has_no_dup_pairs(x_from, x_to),
    weights = has_complete_weights(x_from, x_weights)
  ) 
  all(x_props)
}
