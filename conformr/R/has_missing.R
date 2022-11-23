# Generated from _main.Rmd: do not edit by hand

#' Flags NA in Source Data
#'
has_missing <- function(.data){
  is_miss <- .data |>
    anyNA()

  result <- list(fail=is_miss)

  return(result)
}
