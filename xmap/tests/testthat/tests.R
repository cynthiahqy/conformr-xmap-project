# Generated from create-xmap.Rmd: do not edit by hand  
testthat::test_that(
  "new_xmap_df() accepts arbitrary data.frames with correct from argument",
  {
    df <- data.frame(
      x = letters[1:5],
      y = 1:5,
      z = runif(5)
    )
    xmap <- new_xmap_df(x = df, from = "x", to = "y", weights = "z")
    xmap_attrs <- attributes(xmap)
    testthat::expect_s3_class(xmap, "xmap_df")
    testthat::expect_s3_class(xmap, "xmap")
    testthat::expect_identical(xmap_attrs$col_from, "x")
    testthat::expect_identical(xmap_attrs$col_to, "y")
    testthat::expect_identical(xmap_attrs$col_weights, "z")
    testthat::expect_identical(xmap_attrs$from_set, unique(df$x))
  }
)

testthat::test_that(
  "has_* validation helpers work as expected on valid df",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A2", "B02", 1,
      "A3", "B02", 1,
      "A4", "B03", 0.67,
      "A4", "B04", 0.33
    )
    testthat::expect_false(has_dup_links(df$from, df$to))
    testthat::expect_true(has_complete_weights(df$from, df$weights))
  }
)

testthat::test_that(
  "has_* validation helpers catch invalid df",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A2", "B02", 0.3,
      "A2", "B02", 0.5
    )
    testthat::expect_false(has_complete_weights(df$from, df$weights))
    testthat::expect_true(has_dup_links(df$from, df$to))
  }
)

testthat::test_that(
  "validate_xmap_df() accepts well-formed xmaps",
  {
    df <- tibble::tribble(
      ~node_A, ~node_B, ~w_AB,
      "A1", "B01", 1,
      "A2", "B02", 1,
      "A3", "B02", 1,
      "A4", "B03", 0.25,
      "A4", "B04", 0.75
    )
    x <- new_xmap(df, from = "node_A", to = "node_B", weights = "w_AB")
    out <- testthat::expect_invisible(validate_xmap_df(x))
    testthat::expect_identical(out, x)
  }
)

## columns present
testthat::test_that(
  "validate_xmap_df() rejects missing columns",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1
    )
    x <- new_xmap_df(df, "from", "missing_col", "weights")
    testthat::expect_error(df_check_cols(df, c("from", "missing_col", "weights")),
      class = "abort_missing_cols"
    )
    testthat::expect_error(validate_xmap_df(x),
      class = "abort_missing_cols"
    )
  }
)

## any NA values
testthat::test_that(
  "validate_xmap_df() rejects missing values",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B2", NA,
      NA, "B2", NA,
      "A3", "B1", 1
    )
    x <- new_xmap_df(df, "from", "to", "weights")
    testthat::expect_error(validate_xmap_df(x), class = "abort_na")
  }
)


## column type
testthat::test_that(
  "validate_xmap_df() rejects non-numeric weight columns",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A4", "B03", 0.25,
      "A4", "B04", 0.75
    ) |>
      dplyr::mutate(weights = as.character(weights))
    testthat::expect_error(df_check_col_type(df, "weights"),
      class = "abort_col_type"
    )
    x <- new_xmap_df(df, "from", "to", "weights")
    testthat::expect_error(validate_xmap_df(x),
      class = "abort_col_type"
    )
  }
)

## from set check
testthat::test_that(
  "validate_xmap_df() rejects mismatching from_set",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A4", "B03", 0.25,
      "A4", "B04", 0.75
    )
    bad_set <- c("bad set", "of", "nodes")
    testthat::expect_error(df_check_from_set(df, "from", bad_set))
    x <- new_xmap_df(df, "from", "to", "weights", from_set = bad_set)
    testthat::expect_error(validate_xmap_df(x))
  }
)

## duplicate links
testthat::test_that(
  "validate_xmap_df() rejects duplicate from-to links",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B02", 0.3,
      "A1", "B02", 1
    )
    testthat::expect_error(df_check_links(df, "from", "to"))
    x <- new_xmap_df(df, from = "from", to = "to", weights = "weights")
    testthat::expect_error(validate_xmap_df(x), class = "abort_dup")
  }
)

## complete weights
testthat::test_that(
  "validate_xmap_df() rejects invalid weights",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 0.4,
      "A1", "B02", 0.59
    )
    testthat::expect_error(df_check_weights(df, "from", "weights"),
      class = "abort_weights"
    )
    x <- new_xmap_df(df, "from", "to", "weights")
    testthat::expect_error(validate_xmap_df(x), class = "abort_weights")
  }
)

