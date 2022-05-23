## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(conformr)
library(dplyr)

## ----basic-setup--------------------------------------------------------------
## TODO: write panel_map with four cases

## TODO: write toy data-set


## ----coverage-check-----------------------------------------------------------
## TODO: code demo for coverage-check


## ----na-to-zero---------------------------------------------------------------
## TODO: convert NA to zero in data

## ----data-map-----------------------------------------------------------------
## TODO: data-map using dplyr::left_join()

## ----aggregate-data-map-------------------------------------------------------
## TODO: code demo of aggregation step

## ----use_panel_map------------------------------------------------------------
code_in <- sym("std_A")

# get data
data_in <- conformr:::toy_AB$data_in %>%
  select(country, 
         std_A,
         A_100)

# make panel_map
code_dict <- conformr:::toy_AB$codes_BA
toy_pm <- code_dict %>% 
  make_panel_map_equal(.,  code_in = !!code_in, code_out = std_B, "split_in")

# use panel_map
data_out <- 
  use_panel_map(map = toy_pm,
                data = group_by(data_in, country), value_from = A_100,
                from_code = std_A, to_code = std_B,
                weights = split_in)

## ---- eval=FALSE--------------------------------------------------------------
#  ## TODO: add toy example for pivot long, then transform
#  
#  data %>%
#      dplyr::select(code_from, {{values_from}}) %>%
#    # pivot_longer to turn values_from into single column
#      tidyr::pivot_longer(., {{values_from}}, names_to = "from_name", values_to = "value_in")
#  

## ----grouped-transformation---------------------------------------------------
# write sample data
gdp <- 

# write panel_map
tribble(~year, ~country_in, ~country_out,
        
        )



## ----multi-step---------------------------------------------------------------
# first conversion step


# second conversion step

## ----fruit-NA-----------------------------------------------------------------
# example panel map
fruit_pm <- tribble(~old, ~new, ~weight,
                    "apples", "fruit", 1,
                    "bananas", "fruit", 1,
                    "oranges", "fruit", 1,
                    "carrot", "veg", 1,)

# example data with NA
fruit_data <- tribble(~product, ~sales,
                      "apples", 3250,
                      "bananas", NA,
                      "oranges", 750,
                      "carrot", NA)

# join panel map and data
data_map <- fruit_pm %>%
  rename(product = old) %>%
  full_join(x = ., 
            y = fruit_data,
            by = "product") %>%
  mutate(sales_new = weight * sales) %>%
  rename(category = new)

## ----fruit-NA-preserve--------------------------------------------------------
# "preserve" missingness?
data_map %>%
  group_by(category) %>%
  summarise(sales = sum(sales_new, na.rm=FALSE))

## ----fruit-NA-convert---------------------------------------------------------
# throw away NAs na.rm=TRUE
data_map %>%
  group_by(category) %>%
  summarise(sales = sum(sales_new, na.rm=TRUE))

## ----fruit-NA-greedy----------------------------------------------------------
# preserve NA 
data_map %>%
  group_by(category) %>%
  summarise(sales = sum_greedy(sales_new))

