# Generated from _main.Rmd: do not edit by hand

#' Flag if data set is not completely cover by panel map
#' 
#' @inheritParams use_panel_map
#'  
has_coverage <- function(.data, .map, .from){
  
  missing_links <- .data |>
    dplyr::select(tidyselect::all_of(.from)) |>
    dplyr::distinct() |>
    dplyr::anti_join(.map, by = .from)

  is_covered <- (nrow(missing_links) == 0)

  results <- list(fail=!is_covered,
                  table=missing_links)

  return(results)
}
