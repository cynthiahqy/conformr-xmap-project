## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
data_in <- conformr:::toy_AB$data_in
good_codes <- conformr:::toy_AB$df_codes
good_dict <- conformr:::toy_AB$code_dict %>% dplyr::select(code_B, code_A, n_dest, weight)

dup_codes <- rbind(good_codes, good_codes[1,]) ## make duplicate code mapping
dropA_dict <- code_dict[code_dict$code_A != "x001",] ## remove code instruction

## check_cd_coverage ----
test_that("incomplete `code_dict` raises check_cd_coverage() warning", {
  expect_warning(check_cd_coverage(data = data_in, code_dict = dropA_dict, code_from = "code_A"))
})

## check_cd_weights ----



## make_cd_equal ----
test_that("duplicate code instructions are caught", {
  expect_error(make_cd_equal(dup_codes, code_from = code_A, code_to = code_B))
})

test_that("make_cd_equal makes same weights as toy_AB", {
  fnc_out <- make_cd_equal(good_codes, code_from = code_A, code_to = code_B)
  expect_identical(fnc_out, good_dict)
})

# TODO: write tests using toy_AB data

