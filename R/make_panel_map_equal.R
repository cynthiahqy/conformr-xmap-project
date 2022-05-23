#' Helper to build equal split panel map
#'
#' Generate panel map using all *distinct* correspondences between two classifications.
#'
#' @param code_dict Data frame containing correspondence between source and destination codes
#' @param code_in Variable in `code_dict` containing source codes to convert from.
#' @param code_out Variable in `code_dict` containing destination codes to convert to.
#' @param .weights_to (optional) new column name for storing weights that will be applied to
#' input values.
#'
#' @return Returns panel map
#' @export
#'
#' @examples
make_panel_map_equal <- function(code_dict, code_in, code_out, .weights_to = NULL){

  ## get distinct correspondences
  code_dict <- code_dict %>%
    dplyr::distinct({{code_in}}, {{code_out}})

  ## code names as strings

  ## make column name for weights
  .weights_to <- .weights_to %||% paste("split", deparse(substitute(code_in)), sep = "_")

  panel_map <- code_dict %>%
    dplyr::group_by({{code_in}}) %>%
    dplyr::mutate("n_dest" = dplyr::n(), ## faster than n_distinct()
                  !!.weights_to := 1 / n_dest) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n_dest) # %>%
    # TODO: conformr::as_panel_map()

  return(panel_map)
}
