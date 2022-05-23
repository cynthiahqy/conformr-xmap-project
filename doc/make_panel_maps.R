## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----HS1_to_NAICS_6d_panel_map------------------------------------------------
library(concordance)
library(magrittr)
library(dplyr)
library(conformr)

# import the correspondence table
code_dict <- concordance::hs1_naics %>%
  distinct(HS1_6d, NAICS_6d)

## "manual" way
# make a panel map, with equal weight splits
pm_df <- code_dict %>%
  group_by(HS1_6d) %>%
  mutate(n_dest = n(),
         HS1_split = 1/n_dest) %>%
  select(-n_dest)

## conformr way with de-duplication internal
# OR use the built-in to_equal_panel_map() function
panel_map <- conformr::make_panel_map_equal(code_dict, HS1_6d, NAICS_6d)

# check that it is a panel map
panel_map

# TODO: conformr::is_panel_map(HS1_to_NAICS_6d_map)

## ----ASCO-ANZSCO--------------------------------------------------------------
# remotes::install_github("runapp-aus/strayr")
library(strayr)

# download correspondence table from ABS
## get(https://www.ausstats.abs.gov.au/ausstats/subscriber.nsf/0/F30E72E1516495BDCA2575DF001C7441/$File/12200%20anzsco%20first%20edition%20revision%201%20to%20asco%20second%20edition%20correspondence%20tables.xls)

## 

