## setup data
# code_in : std_A
# code_out : std_B
# group_by : country
# value_from : A_100, A_prod
# weights : weight

toy_out <- conformr:::toy_AB$data_out
toy_pm <- conformr:::toy_AB$pm_BA
toy_in <- conformr:::toy_AB$data_in

test_that("fnc returns same data_out as reference", {
  fnc_out <- use_panel_map(map=toy_pm,
                           data=dplyr::group_by(toy_in, country),
                           values_from=c(A_100,A_prod),
                           from_code=std_A,
                           to_code=std_B,
                           weights=weight,
                           .suffix="_out")
  expect_equal(toy_out, fnc_out)
})

