# Generated from _main.Rmd: do not edit by hand

#' Check coverage of panel map over source data
#' 
#' @inheritParams concord
#' @inheritParams use_panel_map
#' 
#' @returns `data_in` if check is successful, throws error otherwise.
#' @examples
#' 
#' /notrun{
#' check_coverage(df, pm, "std_A")
#' }
#' 
#' 
check_coverage <- function(data_in, pm, .from){
  # call flag function
  has_result <- has_coverage(data_in, pm, .from)
  
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
