## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval=FALSE
)

## ----data-retrieval-----------------------------------------------------------
#  library(comtradr)
#  
#  q <- list()
#  
#  q$Germany <- comtradr::ct_search(reporters = "Australia",
#                 partners = c("Germany"),
#                 start_date = 2016,
#                 end_date = 2017,
#                 type = "goods",
#                 trade_direction = "imports",
#                 commod_codes = c("All"))
#  
#  q$Indonesia <- comtradr::ct_search(reporters = "Australia",
#                 partners = c("Indonesia"),
#                 start_date = 2016,
#                 end_date = 2017,
#                 type = "goods",
#                 trade_direction = "imports",
#                 commod_codes = "All")

## -----------------------------------------------------------------------------
#  monthly <- ct_search(reporters = "Australia",
#                 partners = c("Indonesia"),
#                 start_date = 2016,
#                 end_date = 2016,
#                 freq = "monthly",
#                 type = "goods",
#                 trade_direction = "imports",
#                 commod_codes = "Total")

## -----------------------------------------------------------------------------
#  library(dplyr)
#  q$Indonesia %>% glimpse()
#  
#  q$Indonesia %>%
#    filter(aggregate_level == 4, trade_flow == "Import") %>%
#    select(classification, period, reporter_iso, partner_iso, commodity_code, trade_value_usd)

## ----get-concordance, eval=FALSE----------------------------------------------
#  concordance::hs5_hs4 %>%
#    select(ends_with("_4d")) %>%
#    distinct() %>%
#    conformr::make_panel_map_equal(., HS5_4d, HS4_4d) %>%
#    filter(n_dest != 1)

## ----setup--------------------------------------------------------------------
#  library(dplyr)
#  # TODO: highlight code, value 2D transformation.

## ----simple-------------------------------------------------------------------
#  
#  

## ----complex------------------------------------------------------------------
#  toy_AUS <- conformr:::toy_AB$data_map %>%
#    dplyr::filter(country == "AUS")
#  
#  ## use tidy-select to convert multiple variables using the same weights
#  toy_AUS %>% convert(., std_A, std_B, weight, starts_with("A"))

## ----error-warnings-----------------------------------------------------------
#  ## here's a data_map I prepared earlier
#  toy_AB <- conformr:::toy_AB
#  toy_AB$data_map

## ---- error=TRUE--------------------------------------------------------------
#  ## try convert on the data_map
#  toy_AB$data_map %>%
#    conformr::convert(., std_A, std_B, weight, A_100)
#  
#  ## oops, missing groups
#  toy_AB$data_map %>%
#    dplyr::group_by(country) %>%
#    conformr::convert(., std_A, std_B, weight, A_100)
#  
#  ## what if weights are wrong?
#  toy_AB$data_map %>%
#    dplyr::group_by(country) %>%
#    conformr::convert(., std_A, std_B, weight_less, A_100)

## ---- error=TRUE--------------------------------------------------------------
#  ## what if value to be split are not duplicated?
#  (bad_data_map <- toy_AB$data_map %>%
#    dplyr::filter(country == "AUS") %>%
#    dplyr::mutate(A_100 = case_when(std_B == "C3" ~ NA_real_,
#                                        TRUE ~ A_100)))
#  
#  bad_data_map %>%
#    conformr::convert(., std_A, std_B, weight, A_100)

