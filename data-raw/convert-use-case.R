#' ---
#' output: reprex::reprex_document
#' ---

library(dplyr, warn.conflicts = FALSE)

## {tidyverse} workflow
# NOTE: data_A represents ONE instance of some data that is:
#  * collected periodically,
#  * with some kind of disaggregation,
#  * with categorisation/classification that change between collections
# Examples include:
#  * Labour force statistics at the occupation level
#  * Trade/Macro statistics at industry or product level

codeA <- c("x001", "x002", "x003", "x004", "x005")
n_codeA <- length(codeA)
cases <- list(
  AUS = dplyr::tibble(code_A = codeA,
               value_A = rep_len(10, n_codeA)),
  USA = dplyr::tibble(code_A = codeA,
               value_A = rep_len(1000, n_codeA))
)
(data_A <- cases %>% dplyr::bind_rows(.id = "case"))

# NOTE: code_dict_BA represents
# * ONE possible set of correspondences between two classification standards;
# * and ONE set of weights for redistributing values
#   that map to multiple destination codes.
codes_BA <- dplyr::tribble(~ code_B, ~ code_A,
                                "A1", "x001", # one-to-one
                                "B2", "x002", # many-to-one
                                "B2", "x003",
                                "C3", "x004", # one-to-many (4)
                                "C4", "x004",
                                "C5", "x004",
                                "C6", "x004",
                                "C7", "x005", # one-to-many (3)
                                "C8", "x005",
                                "C9", "x005"
                                )
# NOTE: The combo of correspondence and weight
#  is a more "generic" representation of countrycode conversions
#  (e.g. ISO2 to UN-M49) where the transformation weights
#  are effectively 1.
(code_dict_BA <- codes_BA %>%
    dplyr::group_by(code_A) %>%
    dplyr::mutate(n_dest = n_distinct(code_B),
           weight = 1 / n_dest) %>%
    dplyr::ungroup()
)

# Conversion from code_A to code_B involves:
#  * one-to-one: copy value_A as is
#  * many-to-one: copy value_A across [AB_merged], then sum [data_AB]
#  * one-to-many: distribute value_A into multiple code_B categories
(AB_merged <- data_A %>%
  dplyr::group_by(case) %>%
  dplyr::right_join(x = .,
                    y = code_dict_BA,
                    by = c("code_A")) %>%
  dplyr::mutate(weight_value = weight * value_A)
)

(data_AB <- AB_merged %>%
  dplyr::group_by(code_B, .add = TRUE) %>%
  dplyr::summarise(value_B = sum(weight_value), .groups = "drop_last")
)

## {conformr} workflow
# convert() internally checks:
#  * every code_A has at least one code_B instruction (complete correspondence)
#  * all the A-B weights for a single code_A sum to 1 (no creation/loss of value)
if (FALSE) {
  data_A %>%
    dplyr::group_by(case) %>%
    conformr::convert(data = .,
                    code_dict = code_dict_BA,
                    code_from = code_A,
                    code_to = code_B,
                    values_from = value_A,
                    values_to = "value_B",
                    weight_col = weight)
}

