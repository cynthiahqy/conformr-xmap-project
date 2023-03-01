#'
.get_xmap_subclass_attr <- function(subclass = c("xmap_df", "xmap_tbl")){
  subclass <- rlang::arg_match(subclass)

  class_attr <- switch(subclass,
         xmap_df = c("xmap_df", "xmap", "data.frame"),
         xmap_tbl = c("xmap_tbl", "xmap_df", "xmap", "tbl_df", "tbl", "data.frame"),
         stop("Unknown xmap subclass"))
  
  return(class_attr)
}

#' Low Level Constructors for xmap subclasses
#' @param x data-frame like object.
#' @param col_from,col_to,col_weights character strings naming columns containing source nodes, target nodes and numeric weights.
#' @return xmap_df object. Note that this function unclasses tibbles.
#' @name new_xmap
NULL

#' @describeIn new_xmap Construct xmap_df from data.frame
new_xmap_df <- function(x, col_from, col_to, col_weights, from_set = NULL) {
  #' checks argument types
  stopifnot(is.data.frame(x))
  stopifnot(length(col_from) == 1 && is.character(col_from))
  stopifnot(length(col_to) == 1 && is.character(col_to))
  stopifnot(length(col_weights) == 1 && is.character(col_weights))

  #' naively generates `from_set` if it is missing
  from_set <- from_set %||% as.character(unique(x[[col_from]]))
  stopifnot(is.vector(from_set, mode = "character"))

  #' @return `x` with additional subclasses `xmap_df` and `xmap`

  class(x) <- .get_xmap_subclass_attr("xmap_df")
  structure(x,
    col_from = col_from,
    col_to = col_to,
    col_weights = col_weights,
    from_set = from_set
  )
}

#' Low Level Constructors for xmap subclasses
#' @param x data-frame like object.
#' @param col_from,col_to,col_weights character strings naming columns containing source nodes, target nodes and numeric weights.
#' @return xmap_df object. Note that this function unclasses tibbles.
#' @name new_xmap
NULL

#' @describeIn new_xmap Construct xmap_tbl from tibble
new_xmap_tbl <- function(x, col_from, col_to, col_weights, from_set = NULL) {

  x <- new_xmap_df(x, col_from, col_to, col_weights, from_set)
  
  #' @return `x` with additional subclasses `xmap_tbl`, `xmap_df` and `xmap`
  
  class(x) <- .get_xmap_subclass_attr("xmap_tbl")
  
  return(x)
}
