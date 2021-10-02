# set up test inputs ----
## two country case for testing grouping
dm_needs_group <- conformr:::toy_AB$data_map %>%
  dplyr::select(country, std_B, weight, std_A, A_100, A_prod)

## single country data map
good_dm <- conformr:::toy_AB$data_map %>%
  dplyr::select(country, std_B, weight, std_A, A_100, A_prod) %>%
  dplyr::filter(country == "AUS")

## good data & weights/codes
good_data_in <- conformr:::toy_AB$data_in %>%
  dplyr::filter(country == "AUS")

good_weights <- conformr:::toy_AB$weights_BA

## setup for bad inputs
bad_dm <- list()
bad_data_in <- list()
bad_codes <- list()
bad_weights <- list()


# data_map complete checks ----
test_that("grouped data map passes all checks", {
  group_pass_logical <-
    dm_needs_group %>%
    dplyr::group_by(country) %>%
    dm_check_codes(., std_A, std_B) %>%
    dm_check_weights(map = .,
                     code_in = std_A,
                     code_out = std_B,
                     weights = weight,
                     success_fun = assertr::success_logical)

  expect_true(group_pass_logical)
}
)




# dm_check_codes ----

test_that("good data map passes dm_check_codes()",{
  expect_true(dm_check_codes(good_dm,std_A, std_B,
                             success_fun = assertr::success_logical))
  expect_identical(dm_check_codes(good_dm, std_A, std_B,
                                  success_fun = assertr::success_continue),
                   good_dm)
})

## missing code-out for code-in
bad_weights$drop_stdA <- good_weights %>%
  dplyr::filter(std_A != "x2222")
bad_dm$code_in_no_out <- good_data_in %>%
  dplyr::full_join(bad_weights$drop_stdA, by = "std_A")

## missing code_in in data_in
bad_data_in$NA_code <- good_data_in %>%
  dplyr::mutate(std_A = dplyr::case_when(std_A == "x3333" ~ NA_character_,
                                         TRUE ~ std_A))
bad_dm$value_no_code_in <- bad_data_in$NA_code %>%
  dplyr::left_join(good_weights, by = "std_A")

test_that("missing codes raise error in dm_check_codes()", {
  expect_error(dm_check_codes(bad_dm$code_in_no_out, std_A, std_B))
  expect_error(dm_check_codes(bad_dm$value_no_code_in, std_A, std_B))
})

## extra codes are not an issue? // drop rows without values?
bad_codes$extra_stdB <- conformr:::toy_AB$codes_BA %>%
  dplyr::add_row(std_B = "Z1", std_A = "x0987")

bad_dm$extra_codes_no_data <- good_data_in %>%
  dplyr::full_join(bad_codes$extra_stdB, by = "std_A")

# dm_check_weights ----

test_that("good data map passes dm_check_weights()", {
  weights_pass <-
    good_dm %>%
    dm_check_weights(map = .,
                     code_in = std_A,
                     code_out = std_B,
                     weights = weight,
                     success_fun = assertr::success_continue) %>%
    dplyr::select(names(good_dm)) ## drop extra total column

  weights_pass_logical <-
    good_dm %>%
    dm_check_weights(map = .,
                     code_in = std_A,
                     code_out = std_B,
                     weights = weight,
                     success_fun = assertr::success_logical)

  expect_equal(good_dm, weights_pass)
  expect_true(weights_pass_logical)
})

## bad weights
bad_dm$weights <- good_dm %>%
  dplyr::mutate(weight_more = dplyr::case_when(weight == 1 ~ weight,
                                               TRUE ~ weight + 0.01),
                weight_less = dplyr::case_when(
                                       weight == 0.25 ~ 0.2,
                                       TRUE ~ weight)
                )

test_that("bad weights raise error in dm_check_weights()", {
  weights_fail_more <- bad_dm$weights %>%
    dm_check_weights(map = .,
                   code_in = std_A,
                   code_out = std_B,
                   weights = weight_more,
                   error_fun = assertr::error_logical)

  weights_fail_less <- bad_dm$weights %>%
    dm_check_weights(map = .,
                     code_in = std_A,
                     code_out = std_B,
                     weights = weight_less,
                     error_fun = assertr::error_logical)

  expect_false(weights_fail_more)
  expect_false(weights_fail_less)

  expect_error(dm_check_weights(bad_dm$weights, std_A, std_B, weight_less))
}
)








