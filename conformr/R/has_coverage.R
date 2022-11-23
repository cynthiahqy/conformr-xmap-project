# Generated from _main.Rmd: do not edit by hand

#' Flag if data set is not completely cover by panel map
#' 
has_coverage <- function(.data, .map, code_in, code_out){
  str_code_in <- rlang::as_string(rlang::enexpr(code_in))
  
  missing_links <- .data |>
    dplyr::distinct({{code_in}}) |>
    dplyr::anti_join(.map, by = str_code_in)

  is_covered <- (nrow(missing_links) == 0)

  results <- list(fail=!is_covered,
                  table=missing_links)

  return(results)
}

#' Check coverage of panel map over source data
#' 
#' 
check_coverage <- function(data_in, pm, code_in, code_out, values_from){
  has_results <- is_covered(data_in, pm, {{code_in}}, {{code_out}})
}
