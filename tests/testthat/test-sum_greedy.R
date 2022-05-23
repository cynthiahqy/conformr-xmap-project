test_that("sum of vector all NA returns NA", {
  vec_NA <- c(NA, NA, NA)
  # NA_integer required because sum() converts
  # vec to numeric silently
  expect_equal(sum_greedy(vec_NA), NA_integer_)
  expect_equal(sum_greedy(as.double(vec_NA)), NA_integer_)
})

test_that("sum of mixed vector returns sum", {
  vec_mixed <- c(NA, 2, 6)
  expect_equal(sum_greedy(vec_mixed), 8)
})

test_that("sum of all numeric vector returns sum", {
  vec_value <- c(5, 10, 20)
  expect_equal(sum_greedy(vec_value), 35)
})
