# Generated from create-xmap.Rmd: do not edit by hand  
testthat::test_that(
  "validate_xmap() rejects df with too few/many columns",
  {
    big_x <- data.frame(matrix(data = 1, nrow = 3, ncol = 4))
    little_x <- data.frame(matrix(data = 1, nrow = 3, ncol = 2))

    testthat::expect_error(validate_xmap(big_x), class = "xmap_ncol")
    testthat::expect_error(validate_xmap(little_x), class = "xmap_ncol")
  }
)

testthat::test_that(
  "validate_xmap() rejects df if named columns are not found",
  {
    x <- new_xmap(int_dat$input_df,
                  from = "wrong_name",
                  to = "node_B",
                  weights = "w_AB")
    testthat::expect_error(validate_xmap(x),
                           class = "xmap_col_not_found")
  }
)

