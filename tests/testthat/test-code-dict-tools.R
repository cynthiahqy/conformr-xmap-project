## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
data_in <- conformr:::toy_AB$data_in
good_codes <- conformr:::toy_AB$df_codes
good_dict <- conformr:::toy_AB$code_dict %>% dplyr::select(code_B, code_A, weight)

dup_codes <- rbind(good_codes, good_codes[1,]) ## make duplicate code mapping
dropA_dict <- good_dict[good_dict$code_A != "x1111",] ## remove code instruction

## check_cd_coverage ----
test_that("incomplete `code_dict` raises check_cd_coverage() warning", {
  expect_warning(check_cd_coverage(data = data_in, code_dict = dropA_dict, code_from = "code_A"))
})

## check_cd_weights ----



## make_cd_equal ----
test_that("make_cd_equal output matches test dict", {
  fnc_out <- make_cd_equal(dup_codes, code_from = code_A, code_to = code_B, name_weight_col = "weight")
  expect_identical(fnc_out, good_dict)
})

# TODO: write tests using toy_AB data

