check_cd_weights <- function(code_dict, code_from, code_to, weight_col){
  ### no duplicate instruction in code_dict
  assertthat::assert_that(nrow(code_dict) == nrow(dplyr::distinct(code_dict)))
  ### value distribution weights total exactly 1 for each code_A (no value loss or creation)
  t_weight_by_code_from <- code_dict %>%
    dplyr::group_by(!!rlang::sym(code_from)) %>%
    dplyr::summarise(t_weight = sum(!!rlang::sym(weight_col)))
  assertthat::assert_that(all(t_weight_by_code_from$t_weight == 1))
}

check_cd_coverage <- function(code_dict, data, code_from, code_to){
  ### every code in data has a destination instruction in code_dict
  assertthat::assert_that(all(unique(data[code_A]) %in% unique(code_dict[code_A])))
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
