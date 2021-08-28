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

make_cd_equal <- function(codes, code_from, code_to){
  ## [] quosures
  code_from = enquo(code_from)
  code_to = enquo(code_to)

  ## [x] no duplicate entries
  # TODO: switch to warning/ignore as n_distinct in weight creation ensures no double count
  assertthat::assert_that(nrow(dplyr::distinct(codes, !!code_from, !!code_to)) == nrow(codes))

  ## [] create weights
  codes %>%
    dplyr::group_by(!!code_from) %>%
    dplyr::mutate(n_dest = n_distinct(!!code_to), ## do not use nrow in case of dups
                  weight = 1 / n_dest) %>%
    dplyr::ungroup()
}
