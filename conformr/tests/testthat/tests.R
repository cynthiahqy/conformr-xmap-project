# Generated from _main.Rmd: do not edit by hand  
testthat::test_that(
  "has_bad_weights() returns correct flags",
  {
    # good weights
    testthat::expect_false(
      has_bad_weights(equal_pm$pm_BA, std_A, std_B, weight)$fail)
    
    # bad weights
    testthat::expect_true(
      has_bad_weights(equal_pm$bad_weights, std_A, std_B, weight)$fail)
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
      check_weights(equal_pm$bad_weights, std_A, std_B, weight))
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
    testthat::expect_error(check_missing(equal_pm$bad_data))
  }
)

testthat::test_that(
  "has_coverage() returns expected flags",
  {
    ## complete coverage
    testthat::expect_false(has_coverage(equal_pm$data_A, equal_pm$pm_BA, std_A, std_B)$fail)
    
    ## incomplete coverage
    data_extra <- equal_pm$data_A |>
      dplyr::add_row(std_A = "x7777", value_in = 100)
    testthat::expect_true(has_coverage(data_extra, equal_pm$pm_BA, std_A, std_B)$fail)
  }
)

testthat::test_that(
  "concord() raises expected errors",
  {
    ## column missing from data_in
    testthat::expect_error(concord(.data_in = equal_pm$data_A,
                                   .map = equal_pm$pm_BA,
                                   from_code = std_A,
                                   to_code = std_B,
                                   m_weights = weight,
                                   missing_col1, missing_col2),
                           class = "cols_not_found")
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

