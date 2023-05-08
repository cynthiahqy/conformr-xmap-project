# Generated from create-xmap.Rmd: do not edit by hand

#'
.rename_xmap <- function(.xmap, new_col_from, new_col_to, new_col_weights){
  old_cols <- .get_col_attrs.xmap_df(.xmap)
  new_cols <- c(new_col_from, new_col_to, new_col_weights)
  df <- as.data.frame(.xmap)
  names(df)[names(df) %in% old_cols] <- new_cols
  new_xmap_df(df, new_col_from, new_col_to, new_col_weights)
}
