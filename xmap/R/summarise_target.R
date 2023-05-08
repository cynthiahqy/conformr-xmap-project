# Generated from create-xmap.Rmd: do not edit by hand

#' Summarize the composition of target categories.
#'
#' This function summarizes a crossmap based on the target nomenclature, using a
#' weighting variable to calculate the composition of each target category.
#'
#' @param links A data frame or `xmap_df`
#' @inheritParams as_xmap
#' @param composition_to A string specifying the new column to pass the summarised composition into. Default is "composition".
#' @param frac_formula A formula specifying how to calculate the fraction of each
#'   target value using the weighting variable. Default is "{from}*{weights}".
#' @param unit_formula A formula specifying how to calculate the unit of each
#'   target value using the weighting variable. Default is "{from}".
#' @param collapse A string specifying the separator to use when collapsing the
#'   unit values. Default is "+".
#'
#' @return A tibble summarizing the composition of each target value based on
#'   the weighting variable.
#'
#' @export
summarise_target <- function(links, from, to, weights, composition_to, collapse, frac_formula, unit_formula){
  UseMethod("summarise_target")
}

#' @export
#' @rdname summarise_target
#' @examples
#' df <- data.frame(group = c("x", "y", "z", "x", "y", "z"),
#'                  parent = c("A", "A", "A", "B", "B", "B"),
#'                  weights = c(0.5, 0.3, 0.2, 0.7, 0.1, 0.2))
#' summarise_target(df, group, parent, weights)
#' mock$xmap_abc |> summarise_target()
#' @importFrom dplyr mutate group_by summarise rename case_when
#' @importFrom glue glue_collapse
summarise_target.data.frame <- function(
  links, from, to, weights,
  composition_to = "composition",
  collapse = "+",
  frac_formula = "{from}*{weights}",
  unit_formula = "{from}"
) {
  ## rename
  r_df <- links |>
    dplyr::rename(from = {{from}},
                  to = {{to}},
                  weights = {{weights}})
  ## summarise
  r_df |>
    dplyr::mutate(part = dplyr::case_when(
      weights == 1 ~ glue::glue(unit_formula),
      weights < 1 ~ glue::glue(frac_formula))
    ) |>
    dplyr::group_by(to) |>
    dplyr::summarise(composition = glue::glue_collapse(part, collapse))
}

#' @export
#' @rdname summarise_target
summarise_target.xmap_df <- function(links,
                                     composition_to = "composition",
                                     collapse = "+",
                                     frac_formula = "{from}*{weights}",
                                     unit_formula = "{from}"
                                     ){
  df <- links |>
    xmap_drop_extra() |>
    .rename_xmap("from", "to", "weights") |>
    as.data.frame()

  sum_df <- summarise_target.data.frame(df, from, to, weights,
                              composition_to, collapse,
                              frac_formula, unit_formula)

  names(sum_df)[names(sum_df) == "to"] <- .get_col_attrs.xmap_df(links)$col_to
  return(sum_df)
}
