library(dplyr)

# ---- Toy data for convert() ----
# IDEAL CASE
## [x] code_dict: cover all mapping types
## [x] data_in: include case variable

# BREAKING CASES
## [x] data_in: include BAD non-numeric value column
## [x] code_dict: include BAD weights -- sum > 1
## [x] code_dict: include BAD weights -- sum < 1?
## [>] code_dict: remove mapping for one origin code in TEST
## [>] code_dict: include duplicate mapping in TEST

codeA <- c("x001", "x002", "x003", "x004", "x005")
n_codeA <- length(codeA)
cases <- list(
  ABC = dplyr::tibble(code_A = codeA,
               value_A = rep_len(10, n_codeA),
               value_bad = stringi::stri_rand_strings(n_codeA, 5)),
  XYZ = dplyr::tibble(code_A = codeA,
               value_A = rep_len(1000, n_codeA),
               value_bad = stringi::stri_rand_strings(n_codeA, 5))
)

data_A <- cases %>% dplyr::bind_rows(.id = "case")

## make test correspondence table
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

## generate code_dict with weights
eps <- 0.001 # small "dust" to mess up weights

code_dict_BA <- codes_BA %>%
    dplyr::group_by(code_A) %>%
    dplyr::mutate(n_dest = n_distinct(code_B),
           weight = 1 / n_dest) %>%
    dplyr::ungroup() %>%
  # add bad weights
    dplyr::mutate(weight_more = case_when(weight == 1 ~ weight,
                                         TRUE ~ weight + eps),
                  weight_less = case_when(n_dest == 3 ~ 0.33,
                                          n_dest == 4 ~ 0.2,
                                          TRUE ~ weight)
                  )


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
