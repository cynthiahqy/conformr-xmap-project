#' Defaults for NULL values
#'
`%||%` <- function(x, y) if (is.null(x)) y else x
