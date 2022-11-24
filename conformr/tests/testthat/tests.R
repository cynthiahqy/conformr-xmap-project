# Generated from _main.Rmd: do not edit by hand  
testthat::test_that(
  "has_bad_weights() returns correct flags",
  {
    # good weights
    testthat::expect_false(
      has_bad_weights(equal_pm$pm_BA, std_A, std_B, weight)$fail
      )
    # bad weights
    testthat::expect_true(
      has_bad_weights(equal_pm$bad_weights, std_A, std_B, weight)$fail
      )
  }
)
testthat::test_that(
  "check_weights() works as expected",
  {
    # good weights
    testthat::expect_identical(
      check_weights(equal_pm$pm_BA, std_A, std_B, weight), equal_pm$pm_BA)

    # bad weights
    testthat::expect_error(
      check_weights(equal_pm$bad_weights, std_A, std_B, weight),
      class="invalid_weights"
      )
  }
)

testthat::test_that(
  "has_missing() returns expected flags",
  {
    # good weights
    testthat::expect_false(
      has_missing(equal_pm$data_A)$fail
      )
    
    # bad weights
    testthat::expect_true(
      has_missing(equal_pm$bad_data)$fail
      )
  }
)
testthat::test_that(
  "check_missing() works as expected",
  {
    ## good data
    testthat::expect_identical(check_missing(equal_pm$data_A), equal_pm$data_A)
    
    ## bad data
    testthat::expect_error(check_missing(equal_pm$bad_data),
                           class = "vals_na")
  }
)

testthat::test_that(
  "has_coverage() returns expected flags",
  {
    ## complete coverage
    testthat::expect_false(has_coverage(equal_pm$data_A, equal_pm$pm_BA, "std_A")$fail)
    
    ## incomplete coverage
    testthat::expect_true(has_coverage(equal_pm$data_extra, equal_pm$pm_BA, "std_A")$fail)
  }
)

testthat::test_that(
  "check_coverage() works as expected",
  {
    ## complete coverage
    testthat::expect_identical(check_coverage(equal_pm$data_A, equal_pm$pm_BA, "std_A"), equal_pm$data_A)
    ## incomplete coverage
    testthat::expect_error(check_coverage(equal_pm$data_extra, equal_pm$pm_BA, "std_A"),
                           class = "not_covered")
  }
)

testthat::test_that(
  "use_panel_map() works as expected", {
    testthat::expect_identical(
      use_panel_map(.data = equal_pm$data_A,
              .map = equal_pm$pm_BA,
              .from = std_A,
              .to = std_B,
              .weights = weight,
              .vals = c(A_100),
              .suffix = "_out",
              .by = "std_A"),
      equal_pm$data_B
    )
  }
)

testthat::test_that(
  "concord() raises expected errors",
  {
    ## columns not in data_in
    testthat::expect_error(concord(data_in = equal_pm$data_A,
                                   pm = equal_pm$pm_BA,
                                   from_code = std_A,
                                   to_code = std_B,
                                   m_weights = weight,
                                   values_from = c(missing_col1, missing_col2)
                                   ),
                           class="vals_not_found")
    ## missing values in data_in
    testthat::expect_error(concord(equal_pm$bad_data, equal_pm$pm_BA, std_A, std_B, weight,
                                   values_from = c(A_100)
                                   ),
                           class="vals_na"
                           )
    ## invalid weights are flagged
    testthat::expect_error(concord(equal_pm$data_A, equal_pm$bad_weights, std_A, std_B, weight,
                                   values_from = c(A_100)
                                   ),
                           class="invalid_weights"
                           )
  }
)

testthat::test_that(
  "concord() works as expected",
  {
    testthat::expect_identical(concord(data_in = equal_pm$data_A,
                             pm = equal_pm$pm_BA,
                             from_code = std_A,
                             to_code = std_B,
                             m_weights = weight,
                             values_from = c(A_100),
                             .suffix = "_out"),
                     equal_pm$data_B
                     )
  }
)

testthat::test_that(
  "make_pm_equal() works",
  {
    testthat::expect_identical(
      make_pm_equal(equal_pm$codes_BA, std_A, std_B, .weights_to = "weight"), equal_pm$pm_BA)
    testthat::expect_no_message(
      make_pm_equal(equal_pm$codes_BA, std_A, std_B, .weights_to = "weight"))
  }
)
testthat::test_that(
  "make_pm_equal() handles duplicate link correctly",
  {
    dup_codes_BA <- rbind(equal_pm$codes_BA, equal_pm$codes_BA[1, ])
    testthat::expect_message(
      make_pm_equal(dup_codes_BA, std_A, std_B)
    )
    testthat::expect_identical(
      make_pm_equal(dup_codes_BA, std_A, std_B, .weights_to = "weight"), equal_pm$pm_BA
    )
  }
)

