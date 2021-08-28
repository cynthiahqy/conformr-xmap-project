## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
### single case
case_in <- conformr:::toy_AB$data_in %>%
  dplyr::filter(case == "ABC")
### grouped cases
group_in <- conformr:::toy_AB$data_in %>%
  dplyr::group_by(case)
### code_dict
code_dict <- conformr:::toy_AB$code_dict
code_miss <- code_dict %>% dplyr::filter(code_A != "x004")

## ideal case ----
test_that("convert() output matches toy output", {
  data_out_fnc <- conformr::convert(data = group_in,
                      code_dict = conformr:::toy_AB$code_dict,
                      code_from = "code_A",
                      code_to = "code_B",
                      values_from = value_A,
                      values_to = "value_B",
                      weight_col = weight)

  expect_identical(conformr:::toy_AB$data_out, data_out_fnc)
})

## bad inputs ----
test_that("convert() breaks if values_from is not numeric", {
  expect_error(conformr::convert(data = case_in,
                                 code_dict = code_dict,
                                 code_from = "code_A",
                                 code_to = "code_B",
                                 values_from = "value_bad", #! BAD INPUT !#
                                 values_to = "value_B",
                                 weight_col = "weight"))
})

test_that("convert() breaks if total weights don't sum to 1", {
  expect_error(conformr::convert(data = case_in,
                                 code_dict = code_dict,
                                 code_from = "code_A",
                                 code_to = "code_B",
                                 values_from = "value_A",
                                 values_to = "value_B",
                                 weight_col = "weight_more" #! BAD INPUT !#
                                 )
               )
  expect_error(conformr::convert(data = case_in,
                                 code_dict = code_dict,
                                 code_from = "code_A",
                                 code_to = "code_B",
                                 values_from = "value_A",
                                 values_to = "value_B",
                                 weight_col = "weight_less" #! BAD INPUT !#
  )
  )
})

test_that("convert() breaks if to-from mapping is missing", {

  expect_error()
})

test_that("convert() breaks if code_from isn't found", {
  expect_error(conformr::convert(data = case_in,
                                 code_dict = code_dict,
                                 code_from = "!!---MISTAKE---!!",
                                 code_to = "code_B",
                                 values_from = "value_A",
                                 values_to = "value_B",
                                 weight_col = "weight"))

})
