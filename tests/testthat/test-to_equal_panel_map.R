test_that("returned panel map passes verification", {
  codes_BA <- conformr:::toy_AB$codes_BA
  pm_BA <- to_equal_panel_map(codes_BA)

  expect_equal(is_panel_map(pm_BA), TRUE)
})

test_that("split weights are calculated using distinct rows", {
  # TODO
  # codes_duplicate <-
  # codes_no_dups <-
  # pm_dup <- to_equal_panel_map(codes_duplicate)
  # pm_no_dups <- to_equal_panel_map(codes_no_dups)
  # expect_equal(pm_no_dups, pm_dup)
})
