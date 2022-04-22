#' Check code-dict coverage
#'
#' Checks if every `code_in` in `data` has
#' at least one destination (row) in `code_dict`.
#' Warns if any `data$code_in` is not present in `code_dict$code_in`
#'
#' @param data <df> with data to be converted. Must contain `code_in` column.
#' @param code_dict <dataframe> with instructions for `code_in` into target schema.
#' @param code_in col containing codes to be converted
#'
#' @return
#' @export
#'
#' @family code_dict helpers
#'
#' @examples
check_cd_coverage <- function(data, code_dict, code_in){
  enexpr <- rlang::enexpr
  ### every code in data has a destination instruction in code_dict
  # unique codes
  data_codes <- sort(unique(data[[code_in]]))
  cd_codes <- sort(unique(code_dict[[code_in]]))

  # coverage checks
  ## at least one instruction
  covered <- data_codes %in% cd_codes
  n_not_covered <- sum(!covered)

  # messages
  if (all(covered)){
    rlang::inform(message = c("All unique `data$code_in` are found in `code_dict$code_in`"))
  } else {
    rlang::warn(message = c("Can't find some `data$code_in` in `code_dict$code_in`:")
    )
  }
}

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

