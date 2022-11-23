# Generated from _main.Rmd: do not edit by hand

#' Check panel map weights are valid
#' 
#' Checks if `code_in`, `code_out` and `weights` columns of data frame forms a valid panel map.
#' 
#' @param pm Data Frame containing weighted links `weights` between `code_in` and `code_out`.
#' @inherit make_pm_equal
#' @param weights Column containing weights for transforming values from `code_in` to `code_out`
#'
#' @exports 
#' 
#' @returns The original data frame if the check is passed and an error if not.
check_weights <- function(pm, code_in, code_out, weights){
  
  has_result <- has_bad_weights(pm, {{code_in}}, {{code_out}}, {{weights}})
  
  if (has_result$fail){
    
    cli::cli_abort(c(
        "{.var weights} for each {.var code_in} must sum to 1",
        ""
        ))
  } else {
    return(pm)
  }
}
