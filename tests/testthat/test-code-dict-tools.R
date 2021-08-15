## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
codes <- conformr:::toy_AB$df_codes
code_dict <- conformr:::toy_AB$code_dict

dup_codes <- rbind(codes, codes[1,]) ## make duplicate code instruction

## make_cd_equal
test_that("duplicate code instructions are caught", {
  expect_error(make_cd_equal(dup_codes, code_from = code_A, code_to = code_B))
})

test_that("make_cd_equal makes same weights as toy_AB", {
  fnc_out <- make_cd_equal(codes, code_from = code_A, code_to = code_B)
  compare_cd <- code_dict %>% select(code_B, code_A, n_dest, weight)
  expect_identical(fnc_out, compare_cd)
})

## check_cd_

# TODO: write tests using toy_AB data
