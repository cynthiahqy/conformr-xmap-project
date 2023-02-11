# Generated from create-xmap.Rmd: do not edit by hand

#' Reverse xmap direction
#'
#' @param x xmap object to be reversed
#' @param weights_to A string specifying the name of a new or existing column to store reverse weights in. 
#'
#' @return
#' @export
#'
#' @examples
xmap_reverse <- function(x, weights_to){
  UseMethod("xmap_reverse")
}

#' @describeIn xmap_reverse Reverse a `xmap_df`
#' @return xmap_df
#' 
#' @export
xmap_reverse.xmap_df <- function(x, weights_to = "r_weights"){
  stopifnot(inherits(x, "xmap_df"))
  x_attrs <- attributes(x)
  df <- as.data.frame(x)
  
  ## check xmap can be reversed
  df_check_reversible(df, x_attrs$col_to)

  ## make new xmap
  df[[weights_to]] <- 1
  new_from <- x_attrs$col_to
  new_to <- x_attrs$col_from
  new_weights <- weights_to
  new_cols <- c(new_from, new_to, new_weights)
  ## rearrange columns
  col_order <- c(new_cols, setdiff(names(df), new_cols))
  df <- df[col_order]
  
  ## construction
  xmap <- new_xmap_df(df, new_from, new_to, new_weights)
  
  ## validation
  validate_xmap_df(xmap)
  
  return(xmap)
}

#' @describeIn xmap_reverse Reverse a `xmap_tbl`
#' @return xmap_tbl
#'
#' @export
xmap_reverse.xmap_tbl <- function(x, weights_to = "r_weights"){
  xmap <- xmap_reverse.xmap_df(x, weights_to)
  class(xmap) <- .get_xmap_subclass_attr("xmap_tbl")
  return(xmap)
}
