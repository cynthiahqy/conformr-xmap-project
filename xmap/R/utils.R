#' Defaults for NULL values
#' @name op-null-default
`%||%` <- function(x, y) if (is.null(x)) y else x
