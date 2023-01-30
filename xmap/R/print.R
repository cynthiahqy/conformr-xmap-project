#'
.get_link_types.xmap_df <- function(xmap_df){
  x_attrs <- attributes(xmap_df)
  x_weights <- xmap_df[[x_attrs$col_weights]]
  x_to <- xmap_df[[x_attrs$col_to]]
  
  flags <- c(
      "recode" = has_recode(x_weights),
      "split" = has_split(x_weights),
      "collapse" = has_collapse(x_to)
    )
  types <- names(flags[flags == TRUE])
  type <- cli::pluralize("{types} {? }")

  return(type)
}

#'
.get_link_direction.xmap_df <- function(xmap_df){
  x_attrs <- attributes(xmap_df)
  direction <- paste0("(", x_attrs$col_from, " -> ", x_attrs$col_to, ") ", 
                      "BY " ,x_attrs$col_weights)
  return(direction)
}


print.xmap_df <- function(xmap_df){
  x_direction <- .get_link_direction.xmap_df(xmap_df)
  x_type <- .get_link_types.xmap_df(xmap_df)

  ## switch for retaining tibble printing
  if (inherits(xmap_df, "tbl")) {
    x_links <- tibble::as_tibble(xmap_df)
  } else {
    x_links <- as.data.frame(xmap_df)
  }

  ## print headers and links
  cat(paste0("xmap_df:\n",  x_type, "\n", x_direction, "\n"))
  print(x_links)
}
