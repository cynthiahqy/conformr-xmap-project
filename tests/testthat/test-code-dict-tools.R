## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
data_in <- conformr:::toy_AB$data_in
good_codes <- conformr:::toy_AB$codes_BA
good_dict <- conformr:::toy_AB$weights_BA %>% dplyr::select(std_B, std_A, weight)

dup_codes <- rbind(good_codes, good_codes[1,]) ## make duplicate code mapping
dropA_dict <- good_dict[good_dict$std_A != "x1111",] ## remove code instruction

## check_cd_coverage ----
test_that("incomplete `code_dict` raises check_cd_coverage() warning", {
  expect_warning(check_cd_coverage(data = data_in, code_dict = dropA_dict, code_in = "std_A"))
})

## check_cd_weights ----



## make_cd_equal ----
test_that("make_cd_equal output matches test dict", {
  fnc_out <- make_cd_equal(dup_codes, code_in = std_A, code_out = std_B, name_weight_col = "weight")
  expect_identical(fnc_out, good_dict)
})

# TODO: write tests using toy_AB data

