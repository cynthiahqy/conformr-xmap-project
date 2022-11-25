# Generated from _main.Rmd: do not edit by hand

#' Apply panel_map to data without checks
#'
#' A wrapper around a `{dplyr}` pipeline that takes a panel_map,
#' joins it with data, and transforms selected variables in that data according to
#' instructions in the panel map. Any groups in `data_in` are preserved.
#'
#' @param .data a Data Frame assumed to meet Source Data conditions
#' @param .map a Data Frame assumed to meet Panel Map conditions
#'
#' @return The output has the following properties:
#' * Groups are taken from `data_in`
#'
use_panel_map <- function(.data, .map, .from, .to, .weights, .vals,
                          .suffix, .by){
  
  # subset data for transformation
  data_in <- .data %>%
    dplyr::select({{.from}}, {{.vals}})

  # merge map and data // use default by= argument
  map_join_data <- dplyr::right_join(x = data_in,
                                     y = .map,
                                     by = .by)

  # apply transformation
  data_out <- map_join_data %>%
    dplyr::mutate(dplyr::across({{ .vals }}, ~ .x * {{ .weights }})) %>%
    dplyr::group_by({{ .to }}, .add = TRUE) %>%
    dplyr::summarise(dplyr::across({{ .vals }}, ~ sum(.x)), .groups = "drop_last")

  # rename
  data_out <- data_out %>%
    dplyr::rename_with(., ~ paste0(.x, .suffix), .cols = {{.vals}})

  return(data_out)
}
