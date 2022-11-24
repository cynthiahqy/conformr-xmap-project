# Generated from _main.Rmd: do not edit by hand

#' Check coverage of panel map over source data
#' 
#' @inheritParams concord
#' 
#' @returns `data_in` if check is successful, throws error otherwise.
#' 
check_coverage <- function(data_in, pm, from_code, to_code){
  # call flag function
  str.from <- rlang::as_string(rlang::enexpr(from_code))
  has_result <- has_coverage(data_in, pm, str.from)
  
  # conditionals
  if(has_result$fail){
    cli::cli_abort(
      "{.var data_in$from_code} has values not covered by {.var pm$from_code}",
      class="not_covered"
    )
  } else {
    return(data_in)
  }
  
}
