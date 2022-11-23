# Generated from _main.Rmd: do not edit by hand

#' Helper to build equal split panel map
#'
#' Generate panel map using all *distinct* correspondences between two classifications.
#'
#' @param code_dict Data frame containing correspondence between source and destination codes
#' @inheritParams check_weights
#' @param .weights_to (optional) new column name for storing weights that will be applied to. The default name is `split_<<code_in>>`.
#' input values.
#'
#' @return Returns panel map as tibble
#' @export
#'
#' @examples
make_pm_equal <- function(code_dict, code_in, code_out, .weights_to = NULL){
  ## check and remove for duplicates
  n_dups <- sum(duplicated(code_dict))
  no_dup_links <- n_dups == 0
  if (!no_dup_links) {
    message("Removing duplicate code_in/code_out rows")
    code_dict <- code_dict |>
    dplyr::distinct({{code_in}}, {{code_out}})
  }

  ## make column name for weights
  .weights_to <- .weights_to %||% paste("split", deparse(substitute(code_in)), sep = "_")

  ## make panel map
  panel_map <- code_dict |>
    dplyr::group_by({{code_in}}) |>
    dplyr::mutate(n_dest = dplyr::n(),
                  !!.weights_to := 1 / n_dest) |>
    dplyr::ungroup() |>
    dplyr::select(-n_dest)

  return(panel_map)
}

#' @rdname make_pm_equal
#' @export
make_panel_map_equal <- make_pm_equal
