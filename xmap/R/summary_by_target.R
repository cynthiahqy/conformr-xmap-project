# Generated from create-xmap.Rmd: do not edit by hand

#' Summarize the composition of target categories.
#'
#' This function summarizes the composition of each target `to` category
#' in a set of `from`, `to`, `weight` links.
#' If a data.frame of links is provided, crossmap properties are not checked.
#' This can be useful when combined with `add_placeholder_weights()` to highlight
#' where weights must be chosen.
#' 
#' @param links A data frame or `xmap_df`.
#' @inheritParams as_xmap
#' @param parts_into A string specifying the new column to pass the summarised composition into. Default is "parts".
#' @param na_placeholder. A string character to replace NA weights with.
#' @param frac_formula A glue formula specifying how to calculate the fraction of each
#'   target value using the weighting variable. Default is "\{from\}*\{weights\}".
#' @param unit_formula A glue formula specifying how to calculate the unit of each
#'   target value using the weighting variable. Default is "\{from\}".
#' @param collapse A string specifying the separator to use when collapsing the
#'   unit values. Default is "+".
#'
#' @return A tibble summarizing the composition of each target value based on
#'   the weighting variable.
#'
#' @export
summary_by_target <- function(links, ...) {
  UseMethod("summary_by_target")
}

#' @export
#' @rdname summary_by_target
#' @examples
#' 
#' mock$xmap_abc |> 
#'   summary_by_target()
#'
#' df <- data.frame(
#'   parent = c("A", "A", "A", "B", "B", "B", "C"),
#'   child = c("x", "y", "z", "x", "y", "z", "k")
#' )
#' df |>
#'   add_placeholder_weights(from = parent, to = child, weights_into = "weights") |>
#'   summary_by_target(from = parent, to = child, weights = weights,
#'                     frac_formula = "{from}*{weights}")
#' @importFrom dplyr mutate group_by summarise rename case_when
#' @importFrom glue glue_collapse
summary_by_target.data.frame <- function(links, from, to, weights,
                                         parts_into = "parts",
                                         collapse = "+",
                                         na_placeholder = "___",
                                         frac_formula = "{from}*{weights}",
                                         unit_formula = "{from}",
                                         warn = TRUE) {
  if (warn) {
    cli::cli_warn(c(
      "Summary completed without verifying crossmap properties.",
      "i" = "To silence set `warn = FALSE`")
    )
  }

  ## evaluate glue formulas
  englue_xmap <- function(string_f, col_from, col_to, col_weights) {
    string_f |>
      gsub("from", col_from, x = _) |>
      gsub("to", col_to, x = _) |>
      gsub("weights", col_weights, x = _)
  }
  col_from <- rlang::englue("{{from}}")
  col_to <- rlang::englue("{{to}}")
  col_weights <- rlang::englue("{{weights}}")
  frac_formula <- englue_xmap(frac_formula, col_from, col_to, col_weights)
  unit_formula <- englue_xmap(unit_formula, col_from, col_to, col_weights)
  
  ## summarise
  links |>
    dplyr::mutate("{col_weights}" := dplyr::coalesce(as.character(.data[[col_weights]]), na_placeholder)) |>
    dplyr::mutate(part = dplyr::case_when(
      {{ weights }} == 1 ~ glue::glue(unit_formula),
      {{ weights }} < 1 ~ glue::glue(frac_formula),
      TRUE ~ glue::glue(frac_formula)
    )) |>
    dplyr::group_by({{ to }}) |>
    dplyr::summarise("{parts_into}" := glue::glue_collapse(part, collapse))
}

#' @export
#' @rdname summary_by_target
summary_by_target.xmap_df <- function(links,
                                      parts_into = "parts",
                                      collapse = "+",
                                      frac_formula = "{from}*{weights}",
                                      unit_formula = "{from}") {
  df <- links |>
    xmap_drop_extra() |>
    .rename_xmap("from", "to", "weights") |>
    as.data.frame()

  sum_df <- summary_by_target.data.frame(
    df, from, to, weights,
    parts_into, collapse,
    frac_formula, unit_formula,
    warn = FALSE
  )

  names(sum_df)[names(sum_df) == "to"] <- .get_col_attrs.xmap_df(links)$col_to
  return(sum_df)
}
