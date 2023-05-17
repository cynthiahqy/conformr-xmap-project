# Generated from create-xmap.Rmd: do not edit by hand

# Generated from create-xmap.Rmd: do not edit by hand

#' Autoplot function for xmap_df objects
#'
#' This function generates a plot of an xmap_df object using the ggraph and ggplot2
#' packages. It visualizes the relationships between nodes and edges in the xmap_df
#' object, with different styles for unit weight and fractional weight links, and
#' prints fractional weights on edges.
#'
#' @param object An xmap_df object.
#' @param ... Additional arguments (currently unused).
#'
#' @importFrom ggraph ggraph geom_edge_diagonal geom_node_label
#' @importFrom ggplot2 scale_fill_brewer coord_flip scale_y_reverse theme_minimal autoplot aes
#' @importFrom tidygraph as_tbl_graph activate mutate centrality_degree
#' @importFrom rlang sym
#'
#' @return ggplot2 object
#' @name autoplot.xmap
#'
#' @examples
#' library(ggplot2)
#' library(ggraph)
#' library(tidygraph)
#' df <- data.frame(from = c("A", "A", "B", "B", "B"),
#'                  to = c("X", "Y", "X", "Y", "Z"),
#'                  weights = c(0.6, 0.4, 0.2, 0.7, 0.1))
#' xmap <- xmap::as_xmap_df(df, from, to, weights)
#' autoplot(xmap)
NULL

#' @rdname autoplot.xmap
#' @export
autoplot.xmap_df <- function(object, ...){

  x_attrs <- attributes(object)

  tidygraph_data <-
    as_tbl_graph(object) |>
  ## calculating edge properties
    tidygraph::activate(edges) |>
    tidygraph::mutate(frac_weight = ifelse(!!sym(x_attrs$col_weights) < 1, TRUE, FALSE)) |>
    tidygraph::mutate(edge_linetype = ifelse(frac_weight, "dashed", "solid")) |>
    tidygraph::mutate(edge_label_pos = ifelse(frac_weight, 0.8, 0.2)) |>
  ## calculating node properties
    tidygraph::activate(nodes) |>
    tidygraph::mutate(n_from = tidygraph::centrality_degree(mode = "in"),
         in_from = n_from == 0,
         collapse = n_from > 1)

  tidygraph_data |>
    ## now we plot...
    ggraph::ggraph(layout = "sugiyama") +
    ## unit weight links,
    ggraph::geom_edge_diagonal(
      aes(edge_linetype = I(edge_linetype),
          edge_alpha = !!sym(x_attrs$col_weights),
          filter = !frac_weight),
      end_cap = circle(6, 'mm'),
      show.legend = FALSE
      ) +
    ## frac weight links,
    ggraph::geom_edge_diagonal(
      aes(edge_linetype = I(edge_linetype),
          edge_alpha = !!sym(x_attrs$col_weights),
          filter = frac_weight,
          label = !!sym(x_attrs$col_weights),
          label_pos = edge_label_pos,
          ),
      end_cap = circle(6, 'mm'),
      show.legend = FALSE,
      angle_calc = "along",
      label_dodge = grid::unit(2, "mm")
      ) +
    ## from nodes,
    ggraph::geom_node_label(aes(label = name,
                                filter=in_from),
                            ) +
    ## to nodes,
    ggraph::geom_node_label(aes(label = name,
                                fill = collapse,
                                filter=!in_from
                                ),
                            show.legend = FALSE,
                            ) +
    ggplot2::scale_fill_brewer() +
    ## and finally modify coordinates, scale and theme
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse() +
    ggplot2::theme_minimal() +
    ggraph::th_no_axes()
}
