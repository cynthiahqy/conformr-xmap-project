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
code_in <- c("x1111", "x2222", "x3333", "x4444", "x5555", "x6666")

data_in <- tidyr::expand_grid(country = c("AUS", "JPN"),
                   std_A = code_in) %>%
  dplyr::mutate(A_100 = 100,
                A_prod = abs(rnorm(nrow(.))) * 10000) %>%
  dplyr::mutate(value_str = stringi::stri_rand_strings(nrow(.), 5)) ## bad value!)


## make test correspondence table
codes_BA <- dplyr::tribble(~ std_B, ~ std_A,
                                "A1", "x1111", # one-to-one
                                "B2", "x2222", # many-to-one
                                "B2", "x3333",
                                "C3", "x4444", # one-to-many (4)
                                "C4", "x4444",
                                "C4", "x6666", # many-to-many
                                "C5", "x4444",
                                "C6", "x4444",
                                "C7", "x5555", # one-to-many (3)
                                "C8", "x5555",
                                "C9", "x5555"
                                )

## generate code_dict with weights
eps <- 0.001 # small "dust" to mess up weights

weights_BA <- codes_BA %>%
    dplyr::group_by(std_A) %>%
    dplyr::mutate(n_dest = dplyr::n_distinct(std_B),
                  weight = 1 / n_dest) %>%
    dplyr::ungroup() %>%
  # add bad weights
    dplyr::mutate(weight_more = dplyr::case_when(weight == 1 ~ weight,
                                         TRUE ~ weight + eps),
                  weight_less = dplyr::case_when(n_dest == 3 ~ 0.33,
                                          n_dest == 4 ~ 0.2,
                                          TRUE ~ weight)
                  )

## generate data_map
data_map_BA <- dplyr::left_join(data_in, weights_BA, by = "std_A")

## final data // collapse destination codes with multiple transfers
data_AB_out <- data_map_BA %>%
  dplyr::group_by(country) %>%
  dplyr::mutate(across(starts_with("A_"), ~ .x * weight)) %>%
  dplyr::group_by(std_B, .add = TRUE) %>%
  dplyr::summarise(dplyr::across(starts_with("A_"), ~ sum(.x), .names = "{.col}_out"),
                   .groups = "drop_last")

toy_AB <- list("data_in" = data_in,
                "codes_BA" = codes_BA,
                "weights_BA" = weights_BA,
                "data_map" = data_map_BA,
                "data_out" = data_AB_out)

# usethis::use_data(toy_AB, internal = TRUE, overwrite = TRUE)
