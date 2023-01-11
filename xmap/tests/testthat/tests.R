# Generated from create-xmap.Rmd: do not edit by hand  
testthat::test_that(
  "new_xmap() works as expected",
  {
    xmap <- new_xmap(x = int_dat$input_df, from = "node_A", to = "node_B", weights = "w_AB")
    xmap_attrs <- attributes(xmap)
    testthat::expect_s3_class(xmap, "xmap_df")
    testthat::expect_identical(xmap_attrs$col_from, "node_A")
    testthat::expect_identical(xmap_attrs$col_to, "node_B")
    testthat::expect_identical(xmap_attrs$col_weights, "w_AB")
    testthat::expect_identical(xmap_attrs$from_set, unique(int_dat$input_df$node_A))
  }
)

testthat::test_that(
  "new_xmap() rejects incorrect object types",
  {
    testthat::local_edition(3)
    testthat::expect_snapshot(error = TRUE, new_xmap(x = "not a data.frame"))
    testthat::expect_snapshot(error = TRUE, new_xmap(from = 33))
    testthat::expect_snapshot(error = TRUE, new_xmap(from = "chr", to = expression(5 + 9)))
    testthat::expect_snapshot(error = TRUE, new_xmap(from = "chr", to = "char", weights = c("dsf", "3925")))
  }
)

## dimension check
testthat::test_that(
  "validate_xmap() rejects too few/many columns",
  {
    big_x <- data.frame(matrix(data = 1, nrow = 3, ncol = 4))
    little_x <- data.frame(matrix(data = 1, nrow = 3, ncol = 2))

    testthat::expect_error(validate_xmap(big_x), class = "xmap_ncol")
    testthat::expect_error(validate_xmap(little_x), class = "xmap_ncol")
  }
)

## columns present
testthat::test_that(
  "validate_xmap() rejects missing columns",
  {
    x <- new_xmap(int_dat$input_df,
                  from = "wrong_name",
                  to = "node_B",
                  weights = "w_AB")
    testthat::expect_error(validate_xmap(x),
                           class = "xmap_col_not_found")
  }
)

## column type
testthat::test_that(
  "validate_xmap() rejects non-numeric weight columns",
  {
    x <- new_xmap(int_dat$input_df,
                  from = "node_A",
                  to = "node_B",
                  weights = "w_AB") |>
      dplyr::mutate(w_AB = as.character(w_AB))
    testthat::expect_error(validate_xmap(x),
                           class = "xmap_col_type")
  }
)

