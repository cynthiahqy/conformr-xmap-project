# Generated from create-xmap.Rmd: do not edit by hand

#'
.calc_link_types.xmap_df <- function(xmap_df){
  x_attrs <- attributes(xmap_df)
  x_weights <- xmap_df[[x_attrs$col_weights]]
  x_to <- xmap_df[[x_attrs$col_to]]
  
  flags <- c(
      "recode" = vhas_recode(x_weights),
      "split" = vhas_split(x_weights),
      "collapse" = vhas_collapse(x_to)
    )
  types <- names(flags[flags == TRUE])
  type <- cli::pluralize("{types} {? }")

  return(type)
}

#'
.calc_link_direction.xmap_df <- function(xmap_df){
  x_attrs <- attributes(xmap_df)
  direction <- paste0("(", x_attrs$col_from, " -> ", x_attrs$col_to, ") ", 
                      "BY " ,x_attrs$col_weights)
  return(direction)
}

#' Print an `xmap` object
#' 
#' @name print.xmap
NULL

#' @describeIn print.xmap Print an `xmap_df`
#'
#' @export
print.xmap_df <- function(x){
  x_direction <- .calc_link_direction.xmap_df(x)
  x_type <- .calc_link_types.xmap_df(x)
  x_links <- as.data.frame(x)

  ## print headers and links
  cat(paste0("xmap_df:\n",  x_type, "\n", x_direction, "\n"))
  print(x_links)
}
