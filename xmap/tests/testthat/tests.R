# Generated from create-xmap.Rmd: do not edit by hand  
testthat::test_that(
  "has_* xmap validation helpers work as expected on valid df",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A2", "B02", 1,
      "A3", "B02", 1,
      "A4", "B03", 0.67,
      "A4", "B04", 0.33
    )
    testthat::expect_true(has_no_dup_pairs(df$from, df$to))
    testthat::expect_true(has_complete_weights(df$from, df$weights))
    testthat::expect_true(has_xmap_props(df$from, df$to, df$weights))
  }
)

testthat::test_that(
  "has_* xmap validation helpers catch invalid df",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A2", "B02", 0.3,
      "A2", "B02", 0.5
    )
    testthat::expect_false(has_complete_weights(df$from, df$weights))
    testthat::expect_false(has_no_dup_pairs(df$from, df$to))
  }
)

testthat::test_that(
  "has_complete_weights() works on recurring fractional weights",
  {
    df <- data.frame(key1 = rep("A1", 3),
                     key2 = c("B01", "B02", "B03"),
                     share = rep(1/3, 3))

    testthat::expect_true(has_complete_weights(df$key1, df$share))
  }
)

testthat::test_that(
  "has_* relation type flag functions work as expected",
  {
    w_1to1 <- rep(1, 10)
    w_1toM <- rep(1/6, 6)
    to_1fromM <- rep("country", 4)
    testthat::expect_true(has_recode(w_1to1))
    testthat::expect_false(has_recode(w_1toM))
    testthat::expect_true(has_split(w_1toM))
    testthat::expect_false(has_split(w_1to1))
    testthat::expect_true(has_collapse(to_1fromM))
  }
)

testthat::test_that(".get_xmap_subclass_attr() rejects unknown subclass",
                    {
                      testthat::expect_error(.get_xmap_subclass_attr("unknown"))
                    })

testthat::test_that(
  "new_xmap_df() accepts arbitrary data.frames with correct from argument",
  {
    df <- data.frame(
      x = letters[1:5],
      y = 1:5,
      z = runif(5)
    )
    xmap <- new_xmap_df(x = df, "x", "y", "z")
    xmap_attrs <- attributes(xmap)
    testthat::expect_s3_class(xmap, .get_xmap_subclass_attr("xmap_df"))
    testthat::expect_identical(xmap_attrs$col_from, "x")
    testthat::expect_identical(xmap_attrs$col_to, "y")
    testthat::expect_identical(xmap_attrs$col_weights, "z")
    testthat::expect_identical(xmap_attrs$from_set, unique(df$x))
  }
)

testthat::test_that("check_col_order() works as expected",
                    {
                      df <- data.frame(a = 1, b = 2, c = 3)
                      testthat::expect_invisible(check_col_order(df, "a", "b", "c"))
                      testthat::expect_identical(check_col_order(df, "a", "b", "c"), df)
                      testthat::expect_error(check_col_order(df, "b", "a", "c"),
                                             class = "abort_col_order")
                    })

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
    x <- new_xmap_df(df, "node_A", "node_B", "w_AB")
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
    testthat::expect_error(abort_missing_cols(df, c("from", "missing_col", "weights")),
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
    testthat::expect_error(check_weights_col_type(df, "weights"),
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
    testthat::expect_error(check_from_set(df, "from", bad_set))
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
    testthat::expect_error(check_dup_pairs(df, "from", "to"), class = "abort_dup_pairs")
    x <- new_xmap_df(df, "from", "to", "weights")
    testthat::expect_error(validate_xmap_df(x), class = "abort_dup_pairs")
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
    testthat::expect_error(check_bad_weights(df, "from", "weights"),
      class = "abort_bad_weights"
    )
    x <- new_xmap_df(df, "from", "to", "weights")
    testthat::expect_error(validate_xmap_df(x), class = "abort_bad_weights")
  }
)

testthat::test_that(
  "as_xmap() is returns expected xmap subclasses",
  {
     tbl_links <- tibble::tribble(
      ~f, ~t, ~w,
      "A1", "B01", 1,
      "A2", "B02", 1,
      "A3", "B02", 1,
      "A4", "B03", 0.67,
      "A4", "B04", 0.33
      )
     df_links <- as.data.frame(tbl_links)
     
     ## default subclasses work as expected
     testthat::expect_s3_class(as_xmap_df(df_links, f, t, w),
                               .get_xmap_subclass_attr("xmap_df"))
     
     ## override subclass works as well
     testthat::expect_s3_class(as_xmap_df(tbl_links, f, t, w, subclass = "xmap_df"),
                               .get_xmap_subclass_attr("xmap_df"))
  }
)

testthat::test_that("xmap_to_matrix handles xmaps with different column counts",{
  links <- tibble::tribble(
    ~f, ~t, ~w,
    "A1", "B01", 1,
    "A2", "B02", 1,
    "A3", "B02", 1,
    "A4", "B03", 0.25,
    "A4", "B04", 0.75
  ) 
  xmap_small <- new_xmap_df(links, "f", "t", "w")

  links_extra <- links |> 
    dplyr::mutate(ex = "extra")
  xmap_extra <- new_xmap_df(links_extra, "f", "t", "w")
  
  xmap_matrix_small <- xmap_small |> xmap_to_matrix()
  xmap_matrix_extra <- xmap_extra |> xmap_to_matrix()
  
  testthat::expect_identical(xmap_matrix_small, xmap_matrix_extra)
  }
  )

testthat::test_that("xmap_to_list works as expected", {
  tar_list <- list(AA = c("x3", "x4", "x6"),
                  BB = c("x1", "x5"),
                  CC = c("x2"))
  xmap_c <- links_from_list(tar_list, "source", "target", "weights") |>
            new_xmap_df("source", "target", "weights")
  out_list <- xmap_to_list(xmap_c)
  testthat::expect_identical(tar_list, out_list)
})

testthat::test_that("xmap_reverse.xmap_df() works as expected",             {
  df_x <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A4", "B03", 0.25,
      "A4", "B04", 0.75
    ) |> as.data.frame() |> 
    new_xmap_df("from", "to", "weights")
  
  df_x_rev <- data.frame(
    to = df_x$to,
    from = df_x$from,
    r_weights = 1
  ) |>
    new_xmap_df("to", "from", "r_weights")
  
  # class checks
  testthat::expect_s3_class(xmap_reverse.xmap_df(df_x), class(df_x_rev))
  testthat::expect_s3_class(xmap_reverse(df_x), class(df_x_rev))
  
  # output checks
  testthat::expect_identical(xmap_reverse.xmap_df(df_x), df_x_rev)
  testthat::expect_identical(df_check_reversible(df_x,"to"), df_x)
}
)

testthat::test_that('xmap_drop_extra works as expected', {
  links <- tibble::tribble(
  ~f, ~t, ~w,
  "A1", "B01", 1,
  "A2", "B02", 1,
  "A3", "B02", 1,
  "A4", "B03", 0.25,
  "A4", "B04", 0.75
  ) 
  xmap_small <- new_xmap_df(links, "f", "t", "w")

  links_extra <- links |> 
    dplyr::mutate(ex = "extra")
  xmap_extra <- new_xmap_df(links_extra, "f", "t", "w")
  
  xmap_drop_df <- xmap_extra |> xmap_drop_extra.xmap_df()
  xmap_drop <- xmap_extra |> xmap_drop_extra()
  
  testthat::expect_identical(xmap_small, xmap_drop_df)
  testthat::expect_identical(xmap_small, xmap_drop)
})

