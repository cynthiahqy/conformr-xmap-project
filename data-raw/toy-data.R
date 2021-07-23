library(dplyr)

# ---- Toy data for convert() ----
# IDEAL CASE
## [x] code_dict: cover all mapping types
## [x] data_in: include case variable

# BREAKING CASES
## [] data_in: include BAD non-numeric value column
## [] code_dict: include BAD weights -- sum > 1
## [] code_dict: remove mapping for one origin code
## [] code_dict: include duplicate mapping

origin_codes <- c("x001", "x002", "x003", "x004")
cases <- list(
  ABC = dplyr::tibble(code_A = origin_codes,
               value_A = rep_len(10, length(origin_codes))),
  XYZ = dplyr::tibble(code_A = origin_codes,
               value_A = rep_len(100, length(origin_codes)))
)

data_A <- cases %>% dplyr::bind_rows(.id = "case")

## make test correspondence table
codes_BA <- dplyr::tribble(~ code_B, ~ code_A,
                                "A1", "x001", # one-to-one
                                "B2", "x002", # many-to-one
                                "B2", "x003",
                                "C3", "x004", # one-to-many
                                "C4", "x004",
                                "C5", "x004",
                                "C6", "x004"
                                )

## generate default weights
code_dict_BA <- codes_BA %>%
    dplyr::group_by(code_A) %>%
    dplyr::mutate(n_dest = n_distinct(code_B),
           weight = 1 / n_dest) %>%
    dplyr::ungroup()

## intermediate merge of code & value transfer weights
AB_merged <- data_A %>%
  dplyr::group_by(case) %>%
  dplyr::right_join(x = .,
                    y = code_dict_BA,
                    by = c("code_A")) %>%
  dplyr::mutate(weight_value = weight * value_A)

## final data // collapse destination codes with multiple transfers
data_AB_out <- AB_merged %>%
  dplyr::group_by(code_B, .add = TRUE) %>%
  dplyr::summarise(value_B = sum(weight_value), .groups = "drop_last")

toy_AB <- list("data_in" = data_A,
                "df_codes" = codes_BA,
                "code_dict" = code_dict_BA,
                "df_merged" = AB_merged,
                "data_out" = data_AB_out)

# usethis::use_data(toy_AB, internal = TRUE)
