# Generated from _main.Rmd: do not edit by hand

#' Flag Bad Mapping Weights
#' 
has_bad_weights <- function(.map, code_in, code_out, weights){
  bad_rows <- .map |>
    dplyr::group_by({{code_in}}) |>
    dplyr::summarise(total = sum({{weights}}),
                     weights = paste({{weights}}, collapse=",")) |>
    dplyr::filter(total != 1)
  
  is_bad <- !(nrow(bad_rows) == 0)

  result <- list(fail = is_bad,
                 table = bad_rows)

  return(result)
}
