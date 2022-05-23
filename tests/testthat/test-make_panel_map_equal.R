## tests use internal data created in data-raw/toy-data.R

## prepare data inputs
# code_in: std_A
# code_out: std_B
good_codes <- conformr:::toy_AB$codes_BA
good_pm <- conformr:::toy_AB$pm_BA
dup_codes <- rbind(good_codes, good_codes[1,]) ## make duplicate code mapping

test_that("make_panel_map_equal(toy_codes) returns toy panel map", {
  # TODO: modify test to use smaller toy_code & toy_pm built internally?
  fnc_out <- make_panel_map_equal(good_codes, std_A, std_B, .weights_to ="weight")
  expect_equal(good_pm, fnc_out)
  #expect_equal(is_panel_map(pm_BA), TRUE)
})

test_that("make_panel_map_equal output matches test pm", {
  fnc_out <- make_panel_map_equal(
    dup_codes, std_A, std_B, .weights_to = "weight")
  expect_identical(fnc_out, good_pm)
})

test_that("split weights are calculated using distinct rows", {
  # TODO
  # codes_duplicate <-
  # codes_no_dups <-
  # pm_dup <- to_equal_panel_map(codes_duplicate)
  # pm_no_dups <- to_equal_panel_map(codes_no_dups)
  # expect_equal(pm_no_dups, pm_dup)
})
