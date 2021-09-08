## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
good_weights <- conformr:::toy_AB$weights_BA
miss_weight <- good_weights %>% dplyr::filter(std_A != "x4444")

### data_map
dm_in <- conformr:::toy_AB$data_map

## ideal cases ----
test_that("convert() output matches toy output", {
  data_out_fnc <- dm_in %>%
    dplyr::group_by(country) %>%
    conformr::convert(data_map = .,
                      code_in = std_A,
                      code_out = std_B,
                      values_from = starts_with("A_"), # tidy-select
                      names_suffix = "_out",
                      weights = weight)

  expect_identical(data_out_fnc, conformr:::toy_AB$data_out)
})

test_that("convert() output matches toy multi-value output", {

}
)

## bad inputs ----
test_that("convert() breaks if values_from is not numeric", {
  expect_error(conformr::convert(data_map = dm_in,
                                 code_in = std_A,
                                 code_out = std_B,
                                 values_from = value_str, #! BAD INPUT !#
                                 weights = weight))
})

test_that("convert() breaks if total weights don't sum to 1", {
  expect_error(conformr::convert(data_map = dm_in,
                                 code_in = std_A,
                                 code_out = std_B,
                                 values_from = A_100,
                                 weights = weight_more #! BAD INPUT !#
                                 )
               )
  expect_error(conformr::convert(data_map = dm_in,
                                 code_in = std_A,
                                 code_out = std_B,
                                 values_from = A_100,
                                 weights = weight_less #! BAD INPUT !#
  )
  )
})

test_that("convert() breaks if to-from mapping is missing", {

  expect_error()
})

test_that("convert() breaks if code_in isn't found", {
  expect_error(conformr::convert(data_map = dm_in,
                                 code_in = "!!---MISTAKE---!!",
                                 code_out = std_B,
                                 values_from = A_100,
                                 weights = weight))

})
