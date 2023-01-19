# Generated from create-xmap.Rmd: do not edit by hand

#' df has no duplicate links
has_no_dup_links <- function(df, from, to){
  links <- df[c(from, to)]
  dup_idx <- anyDuplicated(links)

  !as.logical(dup_idx)
}
