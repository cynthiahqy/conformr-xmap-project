# Generated from create-xmap.Rmd: do not edit by hand

#' Reverse xmap direction
#'
#' @param .xmap xmap object to be reversed
#' @param weights_into A string specifying the name of a new or existing column to store reverse weights in. 
#'
#' @return xmap object of same class as `x`, or throws an error if `x` is not reversible
#' @export
xmap_reverse <- function(.xmap, weights_into){
  UseMethod("xmap_reverse")
}

#' @describeIn xmap_reverse Reverse a `xmap_df`
#' 
#' @export
xmap_reverse.xmap_df <- function(.xmap, weights_into = "r_weights"){
  stopifnot(inherits(.xmap, "xmap_df"))
  x_attrs <- attributes(.xmap)
  df <- as.data.frame(.xmap)
  
  ## check xmap can be reversed
  abort_not_reversible(df, x_attrs$col_to)

  ## make new xmap
  df[[weights_into]] <- 1
  new_from <- x_attrs$col_to
  new_to <- x_attrs$col_from
  new_weights <- weights_into
  new_cols <- c(new_from, new_to, new_weights)
  ## rearrange columns
  #col_order <- c(new_cols, setdiff(names(df), new_cols))
  df <- df[new_cols]
  
  ## construction
  xmap <- new_xmap_df(df, new_from, new_to, new_weights)
  
  ## validation
  validate_xmap_df(xmap)
  
  return(xmap)
}
