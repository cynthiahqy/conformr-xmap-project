``` r
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
#> # A tibble: 10 x 3
#>    case  code_A value_A
#>    <chr> <chr>    <dbl>
#>  1 AUS   x001        10
#>  2 AUS   x002        10
#>  3 AUS   x003        10
#>  4 AUS   x004        10
#>  5 AUS   x005        10
#>  6 USA   x001      1000
#>  7 USA   x002      1000
#>  8 USA   x003      1000
#>  9 USA   x004      1000
#> 10 USA   x005      1000

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
#> # A tibble: 10 x 4
#>    code_B code_A n_dest weight
#>    <chr>  <chr>   <int>  <dbl>
#>  1 A1     x001        1  1    
#>  2 B2     x002        1  1    
#>  3 B2     x003        1  1    
#>  4 C3     x004        4  0.25 
#>  5 C4     x004        4  0.25 
#>  6 C5     x004        4  0.25 
#>  7 C6     x004        4  0.25 
#>  8 C7     x005        3  0.333
#>  9 C8     x005        3  0.333
#> 10 C9     x005        3  0.333

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
#> # A tibble: 20 x 7
#> # Groups:   case [2]
#>    case  code_A value_A code_B n_dest weight weight_value
#>    <chr> <chr>    <dbl> <chr>   <int>  <dbl>        <dbl>
#>  1 AUS   x001        10 A1          1  1            10   
#>  2 AUS   x002        10 B2          1  1            10   
#>  3 AUS   x003        10 B2          1  1            10   
#>  4 AUS   x004        10 C3          4  0.25          2.5 
#>  5 AUS   x004        10 C4          4  0.25          2.5 
#>  6 AUS   x004        10 C5          4  0.25          2.5 
#>  7 AUS   x004        10 C6          4  0.25          2.5 
#>  8 AUS   x005        10 C7          3  0.333         3.33
#>  9 AUS   x005        10 C8          3  0.333         3.33
#> 10 AUS   x005        10 C9          3  0.333         3.33
#> 11 USA   x001      1000 A1          1  1          1000   
#> 12 USA   x002      1000 B2          1  1          1000   
#> 13 USA   x003      1000 B2          1  1          1000   
#> 14 USA   x004      1000 C3          4  0.25        250   
#> 15 USA   x004      1000 C4          4  0.25        250   
#> 16 USA   x004      1000 C5          4  0.25        250   
#> 17 USA   x004      1000 C6          4  0.25        250   
#> 18 USA   x005      1000 C7          3  0.333       333.  
#> 19 USA   x005      1000 C8          3  0.333       333.  
#> 20 USA   x005      1000 C9          3  0.333       333.

(data_AB <- AB_merged %>%
  dplyr::group_by(code_B, .add = TRUE) %>%
  dplyr::summarise(value_B = sum(weight_value), .groups = "drop_last")
)
#> # A tibble: 18 x 3
#> # Groups:   case [2]
#>    case  code_B value_B
#>    <chr> <chr>    <dbl>
#>  1 AUS   A1       10   
#>  2 AUS   B2       20   
#>  3 AUS   C3        2.5 
#>  4 AUS   C4        2.5 
#>  5 AUS   C5        2.5 
#>  6 AUS   C6        2.5 
#>  7 AUS   C7        3.33
#>  8 AUS   C8        3.33
#>  9 AUS   C9        3.33
#> 10 USA   A1     1000   
#> 11 USA   B2     2000   
#> 12 USA   C3      250   
#> 13 USA   C4      250   
#> 14 USA   C5      250   
#> 15 USA   C6      250   
#> 16 USA   C7      333.  
#> 17 USA   C8      333.  
#> 18 USA   C9      333.

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
```

<sup>Created on 2021-07-23 by the [reprex package](https://reprex.tidyverse.org) (v2.0.0)</sup>
