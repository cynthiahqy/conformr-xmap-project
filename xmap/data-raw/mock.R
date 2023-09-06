## code to prepare `mock` dataset goes here

usethis::use_data(mock, overwrite = TRUE)

mock <- list()

mock$named_ctr_iso3c <- countrycode::codelist |>
  dplyr::select(iso3c, iso.name.en) |>
  tidyr::drop_na() |>
  tibble::deframe()

# mock_named$collapse_list <- list(MAMM = c("elephant", "whale", "monkey"),
#                       REPT = c("lizard", "turtle"),
#                       CRUS = c("crab"))

mock$df_anzsco21 <- strayr::anzsco2021 |>
  dplyr::select(tidyselect::starts_with(c("anzsco_major", "anzsco_submajor"))) |>
  dplyr::distinct() |>
  dplyr::select(tidyselect::ends_with("_code"), tidyselect::everything())

mock$xmap_abc <- tibble::tribble(
  ~lower, ~upper, ~share,
  "a", "AA", 1, # one-to-one
  "b", "BB", 1, # one-FROM-many
  "c", "BB", 1,
  "d", "CC", 0.3, # one-to-many
  "d", "DD", 0.6,
  "d", "EE", 0.1
) |>
  xmap::as_xmap_df(from = lower, to = upper, weights = share)

mock$named_aus <- list(AUS = c("AU-NSW", "AU-QLD", "AU-SA", "AU-TAS", "AU-VIC", "AU-WA", "AU-ACT", "AU-NT"))
mock$df_aus_pop <- tibble::tribble(
  ~state_name, ~state, ~pop,
  "New South Wales", "AU-NSW", 8153600,
  "Victoria", "AU-VIC", 6613700,
  "Queensland", "AU-QLD", 5322100,
  "South Australia", "AU-SA", 1820500,
  "Western Australia", "AU-WA", 2785300,
  "Tasmania", "AU-TAS", 571500,
  "Northern Territory", "AU-NT", 250600,
  "Australian Capital Territory", "AU-ACT", 456700
) |>
  dplyr::mutate(ctr = "AUS")
usethis::use_data(mock)
