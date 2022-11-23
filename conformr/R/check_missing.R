# Generated from _main.Rmd: do not edit by hand

#' Checks Source Data for Missing Values
#'
#' @inheritParams concord
#'
#' @export
check_missing <- function(data_in){
  has_result <- has_missing(data_in)
  
  if(has_result$fail){
    cli::cli_abort(
      "{.var data_in} should not have any NA",
      class="vals_na"
    )
  } else {
    return(data_in)
  }
}
