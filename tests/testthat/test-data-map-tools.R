# test inputs

good_dm <- conformr:::toy_AB$data_map %>%
  dplyr::select(country, std_B, weight, std_A, A_100, A_prod)

test_that("correct weights pass check", {
  pipe_pass <-
    good_dm %>%
    dplyr::group_by(country) %>%
    dm_check_weights(data_map = .,
                   code_in = std_A,
                   code_out = std_B,
                   weights = weight) %>%
    dplyr::ungroup()

  msg_pass <- "<informative msg about checks>"

  expect_equal(good_data_map, pipe_pass)
})
