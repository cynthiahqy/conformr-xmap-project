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

usethis::use_data(mock)
