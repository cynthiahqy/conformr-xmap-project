## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
### single case
case_in <- conformr:::toy_AB$data_in %>%
  dplyr::filter(country == "AUS")
### grouped cases
group_in <- conformr:::toy_AB$group_in
### code_dict
code_dict <- conformr:::toy_AB$code_dict
code_miss <- code_dict %>% dplyr::filter(code_A != "x4444")

### data_map
dm_in <- conformr:::toy_AB$data_in %>%
  dplyr::right_join(code_dict, by = "code_A")

## ideal cases ----
test_that("convert() output matches toy output", {
  data_out_fnc <- dm_in %>%
    dplyr::group_by(country) %>%
    conformr::convert(data_map = .,
                      code_in = code_A,
                      code_out = code_B,
                      values_from = starts_with("valA_"), # tidy-select
                      names_suffix = "_out",
                      weights = weight)

  expect_identical(conformr:::toy_AB$data_out, data_out_fnc)
})

test_that("convert() output matches toy multi-value output", {

}
)



## bad inputs ----
test_that("convert() breaks if values_from is not numeric", {
  expect_error(conformr::convert(data = group_in,
                                 code_dict = code_dict,
                                 code_in = "code_A",
                                 code_out = "code_B",
                                 values_from = value_str, #! BAD INPUT !#
                                 weight_col = weight))
})

test_that("convert() breaks if total weights don't sum to 1", {
  expect_error(conformr::convert(data = case_in,
                                 code_dict = code_dict,
                                 code_in = "code_A",
                                 code_out = "code_B",
                                 values_from = valA_100,
                                 weight_col = weight_more #! BAD INPUT !#
                                 )
               )
  expect_error(conformr::convert(data = case_in,
                                 code_dict = code_dict,
                                 code_in = "code_A",
                                 code_out = "code_B",
                                 values_from = valA_100,
                                 weight_col = weight_less #! BAD INPUT !#
  )
  )
})

test_that("convert() breaks if to-from mapping is missing", {

  expect_error()
})

test_that("convert() breaks if code_in isn't found", {
  expect_error(conformr::convert(data = case_in,
                                 code_dict = code_dict,
                                 code_in = "!!---MISTAKE---!!",
                                 code_out = "code_B",
                                 values_from = value_A,
                                 weight_col = weight))

})
