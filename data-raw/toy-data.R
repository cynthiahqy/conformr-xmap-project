library(dplyr)

# ---- Toy data for convert() ----
# IDEAL CASE
## [x] code_dict: cover all mapping types
## [x] data_in: include case variable (country)
## [x] group_in: data_in grouped by case
## [x] multiple values

# BREAKING CASES for tests
## [x] data_in: include BAD non-numeric value column
## [x] code_dict: include BAD weights -- sum > 1
## [x] code_dict: include BAD weights -- sum < 1?
## [>] code_dict: remove mapping for one origin code in TEST
## [>] code_dict: include duplicate mapping in TEST

set.seed(1832)
code_in <- c("x1111", "x2222", "x3333", "x4444", "x5555")

data_in <- tidyr::expand_grid(country = c("AUS", "JPN"),
                   code_A = code_in) %>%
  dplyr::mutate(valA_100 = 100,
                valA_prod = abs(rnorm(nrow(.))) * 10000,
                value_str = stringi::stri_rand_strings(nrow(.), 5))
group_in <- data_in %>% dplyr::group_by(country)


## make test correspondence table
codes_BA <- dplyr::tribble(~ code_B, ~ code_A,
                                "A1", "x1111", # one-to-one
                                "B2", "x2222", # many-to-one
                                "B2", "x3333",
                                "C3", "x4444", # one-to-many (4)
                                "C4", "x4444",
                                "C5", "x4444",
                                "C6", "x4444",
                                "C7", "x5555", # one-to-many (3)
                                "C8", "x5555",
                                "C9", "x5555"
                                )

## generate code_dict with weights
eps <- 0.001 # small "dust" to mess up weights

code_dict_BA <- codes_BA %>%
    dplyr::group_by(code_A) %>%
    dplyr::mutate(n_dest = dplyr::n_distinct(code_B),
           weight = 1 / n_dest) %>%
    dplyr::ungroup() %>%
  # add bad weights
    dplyr::mutate(weight_more = dplyr::case_when(weight == 1 ~ weight,
                                         TRUE ~ weight + eps),
                  weight_less = dplyr::case_when(n_dest == 3 ~ 0.33,
                                          n_dest == 4 ~ 0.2,
                                          TRUE ~ weight)
                  )


## intermediate merge of code & value transfer weights
AB_merged <- group_in %>%
  dplyr::right_join(x = .,
                    y = code_dict_BA,
                    by = c("code_A")) %>%
  dplyr::mutate(across(starts_with("valA_"), ~ .x * weight))

## final data // collapse destination codes with multiple transfers
data_AB_out <- AB_merged %>%
  dplyr::group_by(code_B, .add = TRUE) %>%
  dplyr::summarise(dplyr::across(starts_with("valA_"), ~ sum(.x), .names = "{.col}_out"),
                   .groups = "drop_last")

toy_AB <- list("data_in" = data_in,
               "group_in" = group_in,
                "df_codes" = codes_BA,
                "code_dict" = code_dict_BA,
                "df_merged" = AB_merged,
                "data_out" = data_AB_out)

# usethis::use_data(toy_AB, internal = TRUE, overwrite = TRUE)
