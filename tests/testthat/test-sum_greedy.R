test_that("sum of vector all NA returns NA", {
  vec_NA <- c(NA, NA, NA)
  expect_equal(sum_NA(vec_NA), NA)
  expect_equal(sum_NA(as.double(vec_NA)), NA)
})

test_that("sum of mixed vector returns sum", {
  vec_mixed <- c(NA, 2, 6)
  expect_equal(sum_NA(vec_mixed), 8)
})

test_that("sum of all numeric vector returns sum", {
  vec_value <- c(5, 10, 20)
  expect_equal(sum_NA(vec_value), 35)
})
