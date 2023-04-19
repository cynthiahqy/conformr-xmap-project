# Generated from create-xmap.Rmd: do not edit by hand

#'
.calc_xmap_subclass_attr <- function(subclass = c("xmap_df")){
  subclass <- rlang::arg_match(subclass)

  class_attr <- switch(subclass,
         xmap_df = c("xmap_df", "xmap", "data.frame"),
         stop("Unknown xmap subclass"))
  
  return(class_attr)
}
