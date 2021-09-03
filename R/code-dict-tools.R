#' Check code-dict coverage
#'
#' Checks if every `code_from` in `data` has
#' at least one destination (row) in `code_dict`.
#' Warns if any `data$code_from` is not present in `code_dict$code_from`
#'
#' @param data <df> with data to be converted. Must contain `code_from` column.
#' @param code_dict <dataframe> with instructions for `code_from` into target schema.
#' @param code_from col containing codes to be converted
#'
#' @return
#' @export
#'
#' @family code_dict helpers
#'
#' @examples
check_cd_coverage <- function(data, code_dict, code_from){
  enexpr <- rlang::enexpr
  ### every code in data has a destination instruction in code_dict
  # unique codes
  data_codes <- sort(unique(data[[code_from]]))
  cd_codes <- sort(unique(code_dict[[code_from]]))

  # coverage checks
  ## at least one instruction
  covered <- data_codes %in% cd_codes
  n_not_covered <- sum(!covered)

  # messages
  if (all(covered)){
    rlang::inform(message = c("All unique `data$code_from` are found in `code_dict$code_from`"))
  } else {
    rlang::warn(message = c("Can't find some `data$code_from` in `code_dict$code_from`:")
    )
  }
}

check_cd_weights <- function(code_dict, code_from, code_to, weight_col){
  ### no duplicate instruction in code_dict
  assertthat::assert_that(nrow(code_dict) == nrow(dplyr::distinct(code_dict)))
  ### value distribution weights total exactly 1 for each code_A (no value loss or creation)
  t_weight_by_code_from <- code_dict %>%
    dplyr::group_by(!!rlang::sym(code_from)) %>%
    dplyr::summarise(t_weight = sum(!!rlang::sym(weight_col)))
  assertthat::assert_that(all(t_weight_by_code_from$t_weight == 1))
}

verify_cd <- function(data, code_dict, code_from, code_to, weight_col, correct = FALSE){
  ## at least one instruction per code
  ## instruction is complete (weight check)
}

#' Create code map from code correspondence with equal weight splits
#'
#' Generate code map for transforming values between classification codes,
#' using all distinct correspondence between two codes.
#'
#' @param codes df containing correspondence btween input and output codes
#' @param code_from column name of input code
#' @param code_to column name of output
#'
#' @return
#' @export
#'
#' @examples
make_cd_equal <- function(codes, code_from, code_to, name_weight_col = NULL){

  ## get distinct correspondences
  in_out <- codes %>%
    dplyr::distinct({{code_from}}, {{code_to}})

  ## code names as strings

  ## make column name for weights
  name_weight_col <- name_weight_col %||% paste("w", deparse(substitute(code_from)), sep = "_")

  code_map <- in_out %>%
    dplyr::group_by({{code_from}}) %>%
    dplyr::mutate("n_dest" = dplyr::n(), ## faster than n_distinct()
                  !!name_weight_col := 1 / n_dest) %>%
    dplyr::ungroup() %>%
    dplyr::select(-n_dest)

  return(code_map)
}
