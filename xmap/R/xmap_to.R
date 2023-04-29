# Generated from create-xmap.Rmd: do not edit by hand

#' Extract incidence matrix from xmap objects
#' 
#' Transforms `xmap` objects into incidence matrix where the rows are indexed by the `from` values
#' and the columns are indexed by `to` values. Drops any additional variables.
#' 
#' @param .xmap an xmap object
#' @param sparse logical specifying if the result should be a sparse matrix. Defaults to TRUE.
#' @param ... Reversed for passing arguments to `stats::xtabs`
#' 
#' @return A matrix or sparse matrix object
#' @family {xmap coercion}
#' 
#' @export
xmap_to_matrix <- function(.xmap, sparse, ...) {
  UseMethod("xmap_to_matrix")
}

#' @describeIn xmap_to_matrix Coerce a `xmap_df` to a Matrix
#'
#' @export
#' @examples
#' abc_xmap <- data.frame(
#'  stringsAsFactors = FALSE,
#'                  origin = c("a","b","c","d","e",
#'                           "f","g","h","i","i","j","j","j"),
#'                    dest = c("AA","AA","AA","AA",
#'                           "BB","BB","CC","DD","EE","FF","GG","HH","II"),
#'            link = c(1, 1, 1, 1, 1, 1, 1, 1, 0.5, 0.5, 0.3, 0.3, 0.4)
#'  ) |>
#' as_xmap_df(origin, dest, link)
#' xmap_to_matrix(abc_xmap)
xmap_to_matrix.xmap_df <- function(.xmap, sparse = TRUE, ...){
  x_attrs <- attributes(.xmap)
  df <- .xmap |> as.data.frame(stringsAsFactors = TRUE)
  fm <- paste(x_attrs$col_weights, "~", x_attrs$col_from, "+", x_attrs$col_to,
              collapse = "")
  
  if(sparse){
    x_mtx <- stats::xtabs(stats::as.formula(fm), df, sparse = TRUE)
  } else {
    x_mtx <- stats::xtabs(stats::as.formula(fm), df, sparse = FALSE)
    attr(x_mtx, "call") <- NULL
    unclass(x_mtx)
  }
  return(x_mtx)
}

#' Coerce a unit weight `xmap_df` to a named vector or list
#'
#' Checks that an `xmap` has unit weights, and converts the 
#'   `from` values into:
#'   * a vector for `xmap_to_named_vector()`
#'   * a nested list for `xmap_to_named_list()`
#'   
#' Names are the unique target nodes in `to`,
#'   and each element contains the source node(s) in `from`.
#' 
#' @param .xmap xmap with only unit weights
#'
#' @return Named vector or list.
#' @export
#' @rdname xmap_to_named
#' @family {xmap coercion}
#'
#' @examples
#' iso_vector <- c(AF = "004", AL = "008", DZ = "012", AS = "016", AD = "020")
#' iso_xmap <- iso_vector |> 
#'   as_pairs_from_named(names_to = "iso2c", values_to = "iso3n") |>
#'   add_weights_unit() |>
#'   as_xmap_df(from = iso3n, to = iso2c, weights)
#' identical(iso_vector, xmap_to_named_vector(iso_xmap)) 
xmap_to_named_vector <- function(.xmap){
  stopifnot(is_xmap(.xmap))
  x_attrs <- attributes(.xmap)
  df <- as.data.frame(.xmap)
  # check only unit weights
  w <- df[[x_attrs$col_weights]]
  stop <- !all(w == 1)
  if (stop) {
    cli::cli_abort(msg_abort_frac_weights("Cannot convert to named vector"),
                   class = "abort_frac_weights")
  }
  
  # convert
  df |>
    subset(select = c(x_attrs$col_to, x_attrs$col_from)) |>
    tibble::deframe() |>
    sapply(as.matrix) |>
    sapply(as.vector)
}

#' @rdname xmap_to_named
#' @export
#'
#' @examples
#' animal_list <- list(MAMM = c("elephant", "whale", "monkey"),
#'                  REPT = c("lizard", "turtle"),
#'                  CRUS = c("crab"))
#' animal_xmap <- animal_list |>
#'  as_pairs_from_named(names_to = "class", values_to = "animals") |>
#'  add_weights_unit() |>
#'  as_xmap_df(from = animals, to = class, weights = weights)
#' identical(xmap_to_named_list(animal_xmap), animal_list)
xmap_to_named_list <- function(.xmap) {
  stopifnot(is_xmap(.xmap))
  x_attrs <- attributes(.xmap)
  df <- as.data.frame(.xmap)
  # check only unit weights
  w <- df[[x_attrs$col_weights]]
  
  stop <- !all(w == 1)
  if (stop) {
    cli::cli_abort(msg_abort_frac_weights("Cannot convert to named list"),
                   class = "abort_frac_weights")
  }
  
  # convert
  df |>
    subset(select = c(x_attrs$col_to, x_attrs$col_from)) |>
    tidyr::nest(source = c(x_attrs$col_from)) |>
    tibble::deframe() |>
    sapply(as.matrix) |>
    sapply(as.vector) |>
    as.list()
}
