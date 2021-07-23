test_that("convert() output matches toy output", {
  data_out_fnc <- conformr:::toy_AB$data_in %>%
    dplyr::group_by(case) %>%
    conformr::convert(data = .,
                      code_dict = conformr:::toy_AB$code_dict,
                      code_from = "code_A",
                      code_to = "code_B",
                      values_from = "value_A",
                      values_to = "value_B",
                      weight_col = "weight")

  expect_identical(conformr:::toy_AB$data_out, data_out_fnc)
})
