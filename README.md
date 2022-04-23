# conformr

This package is in active development and not ready for use.
Please follow me on twitter at [@cynthiahqy](https://twitter.com/cynthiahqy) for updates.

`conformr` provides tools
for harmonising multiple data across classifications or statistics 
into a consistent validated dataset.

It is not uncommon in empirical social sciences to encounter "pseudo" [panel data](https://en.wikipedia.org/wiki/Panel_data) (sometimes also called longitudinal data) that has been reported using different standards in different time periods. 
Examples include occupation level labour statistics, or product level trade flows, whereby classification codes are updated periodically to reflect changes in category relevance. Over time categories may be added, removed or even split. 

In order to combine this kind of data into a single panel dataset, researchers must first harmonise the data across classification standards. In particular, researchers must make decisions on how to redistribute values when categories don't have a one-to-one correspondence. These decisions are often hidden in nested for-loops and case statements over each old-new category correspondence. The nature of nested loops make it difficult to review the dataset design, or even validate the data cleaning has been performed as intended.

## From nested-loops to Panel Maps
`conformr` provides a matrix framework for applying transforming data between standards to:
- avoid loss or duplication of data when converting between classifications
- resolve conflicts between reported and calculated statistics
- improve reproducibilty and tractability of combined datasets by
  - facilitating pipelines of multiple sequential concordances
  - providing an tibble-based alternative to nested cases loops for handling one-to-one, one-to-many, and many-to-one correspondences

`conformr` is built upon the realisation that implicit in any transformation between classifications is an application of *weights* to the original values. For example, a transformation from 1 original category to 2 new categories, requires spliting the original value in 2, or alternatively, applying a weight of 0.5 to the original value twice (once each for the 2 new categories).

`conformr` removes the need for nested loops by explicitly stating the weights in a *Panel Map*, which contains both the data to be transformed, and "instructions" on how to transform that data. The "instructions" contain both the code correspondence, as well as what *weights* to use in the transfromation. *Data maps* can be thought of as an extension of standard concordance tables such as those provided in `{concordance}` and `{countrycode}` -- i.e. in addition to *which* target code to "transfer" value to, they also make explicit *how much* value should be assigned to each target code.

A simple workflow (on relatively clean data & code correspondences) would look something like:
```r
## import the data and code correspondences
data_in <- readr::read_csv("some_trade_data.csv")
code_dict <- readr::read_csv("code_correlations.csv")

## generate weights -- simple average in this case
code_weights <- code_dict %>%
  dplyr::group_by(code_in) %>%
  dplyr::mutate(n_dest = n_distinct(code_out),
                weight = 1/n_dest)
                
## make Panel Map
data_map <- dplyr::left_join(x = data_in, y = code_weights,
                             by = "code_in")
                             
## generate transformed data
data_out <- convert(data_map, code_in, code_out, weights, values_from)
```

Some contrived snippets for further illustration (real datasets are much too large to show the various cases):
```r
## code_weights
#> # A tibble: 10 x 4
#>    code_B code_A n_dest weight
#>    <chr>  <chr>   <int>  <dbl>.  ## A-to-B transformation cases
#>  1 A1     x001        1  1       ## one-to-one
#>  2 B2     x002        1  1       ## many-to-one
#>  3 B2     x003        1  1    
#>  4 C3     x004        4  0.25    ## one-to-many
#>  5 C4     x004        4  0.25 
#>  6 C5     x004        4  0.25 
#>  7 C6     x004        4  0.25 
#>  8 C7     x005        3  0.333
#>  9 C8     x005        3  0.333
#> 10 C9     x005        3  0.333

## data_map 
### weight_value shows 1-to-many as value "assignment/transfer"
#> # A tibble: 10 x 7
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

## transformed data_out
### note the aggregation of code_B = B2
#> # A tibble: 9 x 3
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
```
## Planned Features

For implementing & using Panel Maps:
* single step transformation of numeric data between classifications across one-to-one, one-to-many and many-to-one cases (with validation) using Panel Maps: `convert()`. Note that many-to-many transformations are just combinations of the above cases.
* helpers for verifying and creating valid Panel Maps
* helpers for generating custom weights -- e.g. based off some reference dataset
* TBD: an explicit _Panel Map_ object, perhaps as an extension on _tibbles_ (though I have very little idea how to do this, or how to even assess the utility of such an object so any/all advice is welcome! Particularly if you are familar with existing object classes that might overlap with Data Maps)

In addition, to simplying and making explicit data concordance, I also hope to write some additional tools for making explict other design choices that other occur when curating datasets with data at different aggregations:
* helpers for identifying discrepancies between reported and calculated statistics (e.g. `compare` reported category totals vs. calculated sum of category records)
* single step corrections of discrepancies (e.g. `distribute` difference between category members, or `replace` reported totals with calculated sums)
