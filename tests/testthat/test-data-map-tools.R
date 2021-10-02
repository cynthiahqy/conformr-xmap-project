# test inputs

## two country case for testing grouping
good_dm_2 <- conformr:::toy_AB$data_map %>%
  dplyr::select(country, std_B, weight, std_A, A_100, A_prod)

## single country case
good_dm_1 <- good_dm_2 %>%
  dplyr::filter(country == "AUS")
bad_dm_1 <- good_dm_AUS %>%
  dplyr::mutate(weight_more = dplyr::case_when(weight == 1 ~ weight,
                                               TRUE ~ weight + 0.01),
                weight_less = dplyr::case_when(
                                       weight == 0.25 ~ 0.2,
                                       TRUE ~ weight)
                )

test_that("dm_check_weights() returns data if verification passes", {
  weights_pass <-
    good_dm_1 %>%
    dm_check_weights(map = .,
                   code_in = std_A,
                   code_out = std_B,
                   weights = weight,
                   success_fun = assertr::success_continue) %>%
    dplyr::select(names(good_dm_1)) ## drop extra total column

  weights_pass_logical <-
    good_dm_1 %>%
    dm_check_weights(map = .,
                     code_in = std_A,
                     code_out = std_B,
                     weights = weight,
                     success_fun = assertr::success_logical)

  expect_equal(good_dm_1, weights_pass)
  expect_true(weights_pass_logical)
})

test_that("grouped data also works", {
  group_pass_logical <-
    good_dm_2 %>%
    dplyr::group_by(country) %>%
    dm_check_weights(map = .,
                     code_in = std_A,
                     code_out = std_B,
                     weights = weight,
                     success_fun = assertr::success_logical)

  expect_true(group_pass_logical)
}
)

test_that("bad weights raise assertr error fncs", {
  weights_fail_more <- bad_dm_1 %>%
    dm_check_weights(map = .,
                   code_in = std_A,
                   code_out = std_B,
                   weights = weight_more,
                   error_fun = assertr::error_logical)

  weights_fail_less <- bad_dm_1 %>%
    dm_check_weights(map = .,
                     code_in = std_A,
                     code_out = std_B,
                     weights = weight_less,
                     error_fun = assertr::error_logical)

  expect_false(weights_fail_more)
  expect_false(weights_fail_less)
}
)
