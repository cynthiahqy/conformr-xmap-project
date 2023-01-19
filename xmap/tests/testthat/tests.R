# Generated from create-xmap.Rmd: do not edit by hand  
testthat::test_that(
  "new_xmap() accepts arbitrary data.frames with correct from argument",
  {
    df <- data.frame(x = letters[1:5],
                     y = 1:5,
                     z = runif(5))
    xmap <- new_xmap(x = df, from = "x", to = "y", weights = "z")
    xmap_attrs <- attributes(xmap)
    testthat::expect_s3_class(xmap, "xmap_df")
    testthat::expect_identical(xmap_attrs$col_from, "x")
    testthat::expect_identical(xmap_attrs$col_to, "y")
    testthat::expect_identical(xmap_attrs$col_weights, "z")
    testthat::expect_identical(xmap_attrs$from_set, unique(df$x))
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

testthat::test_that(
  "xmap validation helpers work as expected on valid df",
  {
    df <- tibble::tribble(~from, ~to, ~weights,
                           "A1", "B01", 1,
                           "A2", "B02", 1,
                           "A3", "B02", 1,
                           "A4", "B03", 0.67,
                           "A4", "B04", 0.33)
    testthat::expect_true(has_no_NA(df))
    testthat::expect_true(has_no_dup_links(df, "from", "to"))
    testthat::expect_true(has_complete_weights(df, "from", "weights"))
  }
)

testthat::test_that(
  "xmap validation helpers catch invalid df",
  {
    df <- tibble::tribble(~from, ~to, ~weights,
                          "A1", "B01", 1,
                          "A2", "B02", 0.3,
                          "A2", "B02", 0.5)
    testthat::expect_false(has_complete_weights(df, "from", "weights"))
    testthat::expect_false(has_no_dup_links(df, "from", "to"))
    df <- tibble::add_row(df, from = "A3", to = NA, weights = NA)
    testthat::expect_false(has_no_NA(df))
  }
)

testthat::test_that(
  "validate_xmap() accepts well-formed xmaps",
  {
    df <- tibble::tribble(~node_A, ~node_B, ~w_AB,
                           "A1", "B01", 1,
                           "A2", "B02", 1,
                           "A3", "B02", 1,
                           "A4", "B03", 0.25,
                           "A4", "B04", 0.75)
    x <- new_xmap(df, from = "node_A", to = "node_B", weights = "w_AB")
    testthat::expect_invisible(validate_xmap(x))
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
    df <- tibble::tribble(~from, ~to, ~weights,
                           "A1", "B01", 1)
    x <- new_xmap(df,
                  from = "from",
                  to = "missing_col",
                  weights = "weights")
    testthat::expect_error(validate_xmap(x),
                           class = "xmap_col_not_found")
  }
)

## column type
testthat::test_that(
  "validate_xmap() rejects non-numeric weight columns",
  {
    df <- tibble::tribble(~from, ~to, ~weights,
                           "A1", "B01", 1,
                           "A4", "B03", 0.25,
                           "A4", "B04", 0.75)
    x <- new_xmap(df,from = "from",to = "to",weights = "weights") |>
      dplyr::mutate(weights = as.character(weights))
    testthat::expect_error(validate_xmap(x),
                           class = "xmap_col_type")
  }
)

## any NA values
testthat::test_that(
  "validate_xmap() rejects missing values",
  {
    df <- tibble::tribble(~from, ~to, ~weights,
                          "A1", "B2", NA,
                          NA, "B2", NA,
                          "A3", "B1", 1)
    x <- new_xmap(df,from = "from",to = "to",weights = "weights")
    testthat::expect_error(validate_xmap(x), class = "xmap_missing")
  }
)

## duplicate links
testthat::test_that(
  "validate_xmap() rejects duplicate from-to links",
  {
    df <- tibble::tribble(~from, ~to, ~weights,
                          "A1", "B02", 0.3,
                          "A1", "B02", 1)
    x <- new_xmap(df, from="from", to="to", weights="weights")
    testthat::expect_error(validate_xmap(x), class = "xmap_dup")
  }
)

## complete weights
testthat::test_that(
  "validate_xmap() rejects invalid weights",
  {
    df <- tibble::tribble(~from, ~to, ~weights,
                          "A1", "B01", 0.4,
                          "A1", "B02", 0.59)
    x <- new_xmap(df, from="from", to="to", weights="weights")
    testthat::expect_error(validate_xmap(x), class = "xmap_weights")
  }
)

