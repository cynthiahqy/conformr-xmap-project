check_cd_weights <- function(code_dict, code_in, code_out, weight_col){
  ### no duplicate instruction in code_dict
  assertthat::assert_that(nrow(code_dict) == nrow(dplyr::distinct(code_dict)))
  ### value distribution weights total exactly 1 for each std_A (no value loss or creation)
  t_weight_by_code_in <- code_dict %>%
    dplyr::group_by(!!rlang::sym(code_in)) %>%
    dplyr::summarise(t_weight = sum(!!rlang::sym(weight_col)))
  assertthat::assert_that(all(t_weight_by_code_in$t_weight == 1))
}

verify_cd <- function(data, code_dict, code_in, code_out, weight_col, correct = FALSE){
  ## at least one instruction per code
  ## instruction is complete (weight check)
}

