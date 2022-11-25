# Generated from _main.Rmd: do not edit by hand

#' Transform data from Source to Target classification using Panel Map
#' 
#' Currently checks for valid Mapping weights, missing values, and coverage.
#'
#' @param data_in A Data Frame containing the values you want to transform
#' @param pm A Data Frame containing valid Mapping Weights between `from_code` and `to_code`.
#' @param from_code Variable containing Source Codes. Must be present in both `data_in` and `pm`
#' @param to_code Variable in `pm` containing Target Codes.
#' @param m_weights Variable in `pm` containing Mapping Weights.
#' @param values_from A vector of variables in `data_in` to be transformed. E.g. `c(var1, var2)`
#' @param .suffix An (optional) string appended to each `values_from` name to create column names for transformed values.
#' Defaults to `"_out"`
#' 
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' concord(data_in = equal_pm$data_A,
#'        pm = equal_pm$pm_BA,
#'        from_code = std_A,
#'        to_code = std_B,
#'        m_weights = weight,
#'        values_from = c(A_100),
#'        .suffix = "_out")
#' }
#' 
concord <- function(data_in, pm, from_code, to_code, m_weights, values_from, .suffix=NULL){
  
  ## defuse arugments
  str.to <- rlang::as_string(rlang::enexpr(to_code))
  str.from <- rlang::as_string(rlang::enexpr(from_code))
  
  ## check conditions
  pm |> 
    check_weights(code_in = {{from_code}},
                  code_out = {{to_code}},
                  weights = {{m_weights}})
  
  subset_in <- tryCatch(
    data_in |>
    dplyr::select({{from_code}}, {{values_from}}), 
    error = function(cnd) {
      cli::cli_abort(
      "{.var from_code} or {.var values_from} could not be found in {.var data_in}",
      class = "vals_not_found")
    }
  )
  
  subset_in |>
    check_missing()
  
  check_coverage(subset_in, pm, str.from)
  
  ## apply transformation
  # -- create suffix --
  out_suffix <- .suffix %||% paste0("_", str.to)
  join_by <- str.from
  
  data_out <- use_panel_map(.data = subset_in, .map = pm, 
                .from = {{from_code}}, .to = {{to_code}}, .weights = {{m_weights}},
                .vals = {{values_from}}, .suffix = out_suffix,
                .by = join_by)
  
  return(data_out)
}
