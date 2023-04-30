# Generated from create-xmap.Rmd: do not edit by hand  
testthat::test_that("add_weights_*() work as expected",{
  abc_pairs <- data.frame(lower = letters[1:5], upper = LETTERS[1:5])
  abc_links <- data.frame(lower = letters[1:5], upper = LETTERS[1:5], weights = 1)
  testthat::expect_equal(add_weights_unit(abc_pairs), abc_links)
  
  animal_pairs <- list(MAMM = c("elephant", "whale", "monkey"),
                      REPT = c("lizard", "turtle"),
                      CRUS = c("crab")) |>
    as_pairs_from_named("class", "animal")
  animal_links <- animal_pairs |>
    dplyr::group_by(class) |>
    dplyr::mutate(weights = 1/dplyr::n_distinct(animal)) |>
    dplyr::ungroup()
  add_links <- animal_pairs |>
    add_weights_equal(from = class, to = animal, weights_into = "weights")
  
  testthat::expect_equal(animal_links, add_links)
  }
  )

testthat::test_that(
  "vhas_* xmap validation helpers work as expected on valid df",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A2", "B02", 1,
      "A3", "B02", 1,
      "A4", "B03", 0.67,
      "A4", "B04", 0.33
    )
    testthat::expect_true(vhas_no_dup_pairs(df$from, df$to))
    testthat::expect_true(vhas_complete_weights(df$from, df$weights))
    testthat::expect_true(vhas_xmap_props(df$from, df$to, df$weights))
  }
)

testthat::test_that(
  "vhas_* xmap validation helpers catch invalid df",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B01", 1,
      "A2", "B02", 0.3,
      "A2", "B02", 0.5
    )
    testthat::expect_false(vhas_complete_weights(df$from, df$weights))
    testthat::expect_false(vhas_no_dup_pairs(df$from, df$to))
  }
)

testthat::test_that(
  "vhas_complete_weights() works on recurring fractional weights",
  {
    df <- data.frame(key1 = rep("A1", 3),
                     key2 = c("B01", "B02", "B03"),
                     share = rep(1/3, 3))

    testthat::expect_true(vhas_complete_weights(df$key1, df$share))
  }
)

testthat::test_that(
  "vhas_* relation type flag functions work as expected",
  {
    w_1to1 <- rep(1, 10)
    w_1toM <- rep(1/6, 6)
    to_1fromM <- rep("country", 4)
    testthat::expect_true(vhas_recode(w_1to1))
    testthat::expect_false(vhas_recode(w_1toM))
    testthat::expect_true(vhas_split(w_1toM))
    testthat::expect_false(vhas_split(w_1to1))
    testthat::expect_true(vhas_collapse(to_1fromM))
  }
)

testthat::test_that("verify_named_all_1to1() works as expected", {
  v1toM <- c(fruit = "apple", fruit = "banana")
  v1to1 <- c(A = 1, B = 2, C = 3)
  l1toM <- list(fruit = c("apple", "banana"))
  testthat::expect_error(verify_named_all_1to1(v1toM), class = "abort_not_1to1")
  testthat::expect_error(verify_named_all_1to1(l1toM), class = "abort_not_1to1")
  testthat::expect_equal(verify_named_all_1to1(v1to1), v1to1)
  })

testthat::test_that("verify_named_*_unique() work as expected", {
  vdup_pairs <- c(fruit = "apple", fruit = "apple")
  ldup_pairs <- list(fruit = c("apple", "apple"))
  testthat::expect_error(verify_named_all_unique(vdup_pairs), class = "abort_not_unique")
  testthat::expect_error(verify_named_all_unique(ldup_pairs), class = "abort_not_unique")
  vdup_names <- c(fruit = "apple", fruit = "banana")
  ldup_names <- list(fruit = c("apple", "banana"), fruit = "pear")
  testthat::expect_error(verify_named_all_names_unique(vdup_names), class = "abort_not_unique")
  testthat::expect_error(verify_named_all_names_unique(ldup_names), class = "abort_not_unique")
  vdup_values <- c(fruit = "apple", veg = "apple")
  ldup_values <- list(fruit = c("apple", "banana"), veg = "apple")
  testthat::expect_error(verify_named_all_values_unique(vdup_values), class = "abort_not_unique")
  testthat::expect_error(verify_named_all_values_unique(ldup_values), class = "abort_not_unique")
})

testthat::test_that("verify_named_matchset fncs work as expected", {
  v_1to1 <- c(x1 = 1, x2 = 2, x3 = 3)
  refn_exact_1to1 <- c("x1", "x2", "x3")
  refn_subset_1to1 <- c("x1", "x2")
  refn_superset_1to1 <- c("x1", "x2", "x3", "x4")
  testthat::expect_equal(verify_named_matchset_names_exact(v_1to1, refn_exact_1to1), v_1to1)
  testthat::expect_error(verify_named_matchset_names_exact(v_1to1, c("not", "right")),
                         class = "abort_matchset")
  testthat::expect_equal(verify_named_matchset_names_contain(v_1to1, refn_subset_1to1), v_1to1)
  testthat::expect_equal(verify_named_matchset_names_contain(v_1to1, refn_exact_1to1), v_1to1)
  testthat::expect_error(verify_named_matchset_names_contain(v_1to1, refn_superset_1to1),
                         class = "abort_matchset")
  testthat::expect_equal(verify_named_matchset_names_within(v_1to1, refn_superset_1to1), v_1to1)
  testthat::expect_equal(verify_named_matchset_names_within(v_1to1, refn_exact_1to1), v_1to1)
  testthat::expect_error(verify_named_matchset_names_within(v_1to1, refn_subset_1to1),
                         class = "abort_matchset")
  refv_exact_1to1 <- c(1, 2, 3)
  refv_subset_1to1 <- c(1, 2)
  refv_superset_1to1 <- c(1, 2, 3, 4)
  testthat::expect_equal(verify_named_matchset_values_exact(v_1to1, refv_exact_1to1), v_1to1)
  testthat::expect_error(verify_named_matchset_values_exact(v_1to1, c("not", "right")),
                         class = "abort_matchset")
  testthat::expect_equal(verify_named_matchset_values_contain(v_1to1, refv_subset_1to1), v_1to1)
  testthat::expect_equal(verify_named_matchset_values_contain(v_1to1, refv_exact_1to1), v_1to1)
  testthat::expect_error(verify_named_matchset_values_contain(v_1to1, refv_superset_1to1),
                         class = "abort_matchset")
  testthat::expect_equal(verify_named_matchset_values_within(v_1to1, refv_superset_1to1), v_1to1)
  testthat::expect_equal(verify_named_matchset_values_within(v_1to1, refv_exact_1to1), v_1to1)
  testthat::expect_error(verify_named_matchset_values_within(v_1to1, refv_subset_1to1),
                         class = "abort_matchset")
})

testthat::test_that("verify_pairs_* work as expected", {
  v_1to1 <- c(x1 = 1, x2 = 2, x3 = 3)
  pairs_1to1 <- tibble::enframe(v_1to1, "f", "t")
  testthat::expect_identical(verify_pairs_all_1to1(pairs_1to1, f, t), pairs_1to1)
  testthat::expect_identical(verify_pairs_all_unique(pairs_1to1, f, t), pairs_1to1)
}
)

testthat::test_that(".calc_xmap_subclass_attr() rejects unknown subclass",
                    {
                      testthat::expect_error(.calc_xmap_subclass_attr("unknown"))
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
    testthat::expect_s3_class(xmap, .calc_xmap_subclass_attr("xmap_df"))
    testthat::expect_identical(xmap_attrs$col_from, "x")
    testthat::expect_identical(xmap_attrs$col_to, "y")
    testthat::expect_identical(xmap_attrs$col_weights, "z")
    testthat::expect_identical(xmap_attrs$from_set, unique(df$x))
  }
)

testthat::test_that("abort_col_order() works as expected",
                    {
                      df <- data.frame(a = 1, b = 2, c = 3)
                      testthat::expect_invisible(abort_col_order(df, "a", "b", "c"))
                      testthat::expect_identical(abort_col_order(df, "a", "b", "c"), df)
                      testthat::expect_error(abort_col_order(df, "b", "a", "c"),
                                             class = "abort_col_order")
                    })

testthat::test_that(
  "validate & verify xmap fncs accept well-formed xmaps",
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
    testthat::expect_identical(df, verify_links_as_xmap(df, node_A, node_B, w_AB))
  }
)

## columns present
testthat::test_that(
  "validate & verify fncs reject missing columns",
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
    testthat::expect_error(verify_links_as_xmap(df, node_A, node_B, w_AB),
                           class = "abort_missing_cols")
  }
)

## any NA values
testthat::test_that(
  "validate & verify xmap fncs reject missing values",
  {
    df <- tibble::tribble(
      ~from, ~to, ~weights,
      "A1", "B2", NA,
      NA, "B2", NA,
      "A3", "B1", 1
    )
    x <- new_xmap_df(df, "from", "to", "weights")
    testthat::expect_error(validate_xmap_df(x), class = "abort_na")
    testthat::expect_error(verify_links_as_xmap(df, from, to, weights),
                           class = "abort_na")
  }
)


## column type
testthat::test_that(
  "validate & verify xmap fncs reject non-numeric weight columns",
  {
    df <- tibble::tribble(
      ~f, ~t, ~w,
      "A1", "B01", 1,
      "A4", "B03", 0.25,
      "A4", "B04", 0.75
    ) |>
      dplyr::mutate(w = as.character(w))
    testthat::expect_error(abort_weights_col_type(df, "weights"),
      class = "abort_col_type"
    )
    x <- new_xmap_df(df, "f", "t", "w")
    testthat::expect_error(verify_links_as_xmap(df, f, t, w),
                           class = "abort_col_type")
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
    testthat::expect_error(abort_from_set(df, "from", bad_set))
    x <- new_xmap_df(df, "from", "to", "weights", from_set = bad_set)
    testthat::expect_error(validate_xmap_df(x))
  }
)

## duplicate links
testthat::test_that(
  "validate and verify xmap fncs reject duplicate from-to links",
  {
    df <- tibble::tribble(
      ~f, ~t, ~w,
      "A1", "B02", 0.3,
      "A1", "B02", 1
    )
    testthat::expect_error(abort_dup_pairs(df, "f", "t"), class = "abort_dup_pairs")
    x <- new_xmap_df(df, "f", "t", "w")
    testthat::expect_error(verify_links_as_xmap(df, f, t, w),
                           class = "abort_dup_pairs")
  }
)

## complete weights
testthat::test_that(
  "validate & verify xmap fncs rejects invalid weights",
  {
    df <- tibble::tribble(
      ~f, ~t, ~w,
      "A1", "B01", 0.4,
      "A1", "B02", 0.59
    )
    x <- new_xmap_df(df, "f", "t", "w")
    testthat::expect_error(verify_links_as_xmap(df, f, t, w),
                           class = "abort_bad_weights")
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
                               .calc_xmap_subclass_attr("xmap_df"))
     
     ## override subclass works as well
     testthat::expect_s3_class(as_xmap_df(tbl_links, f, t, w, subclass = "xmap_df"),
                               .calc_xmap_subclass_attr("xmap_df"))
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

testthat::test_that("xmap_to_named works as expected", {
  links <- tibble::tribble(
    ~f, ~t, ~w,
    "A1", "B01", 1,
    "A2", "B02", 1,
    "A3", "B02", 1,
    "A4", "B03", 0.25,
    "A4", "B04", 0.75
  )
  ## works for collapse relations
  xmap_unit <- new_xmap_df(links[1:3,], "f", "t", "w")
  unit_list <- list(B01 = c("A1"), B02 = c("A2", "A3"))
  unit_vector <- tibble::deframe(xmap_unit[,c("t", "f")])
  testthat::expect_identical(unit_list, xmap_to_named_list(xmap_unit))
  testthat::expect_identical(unit_vector, xmap_to_named_vector(xmap_unit))
  ## rejects split relations
  xmap_mixed <- new_xmap_df(links, "f", "t", "w")
  testthat::expect_error(xmap_to_named_list(xmap_mixed), 
                         class = "abort_frac_weights")
})

testthat::test_that("xmap_to_named_list() reverses as_pairs_from_named()", {
  link_list <- list(AA = c("x3", "x4", "x6"),
                    BB = c("x1", "x5"),
                    CC = c("x2")
                  )
  link_xmap <-
   as_pairs_from_named(link_list,
                  "capital", "xvars") |>
   add_weights_unit(weights_into = "w") |>
   new_xmap_df("xvars", "capital", "w")
  testthat::expect_identical(xmap_to_named_list(link_xmap), link_list)
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
  testthat::expect_identical(abort_not_reversible(df_x,"to"), df_x)
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
  
  xmap_drop <- xmap_extra |> xmap_drop_extra()
  
  testthat::expect_identical(xmap_small, xmap_drop)
})

