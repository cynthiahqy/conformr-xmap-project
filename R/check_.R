# ---- checker functions ----
check_pm_split_sum <- function(pm, code_in, split_in){
  bad_rows <- pm %>%
    dplyr::group_by({{code_in}}) %>%
    dplyr::summarise(split_total = sum({{split_in}})) %>%
    dplyr::filter(split_total != 1)

  if (nrow(bad_rows) == 0){
    return(pm)
  } else {
    # TODO: add informative error message
    return(bad_rows)
  }
}

check_code_duplicates <- function(codes, code_in, code_out){

}

# ---- Errors ----
error_incomplete_split <- function(pm){

}
