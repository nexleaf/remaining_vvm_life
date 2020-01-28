library(lubridate)
context("Remaining VVM Life")

test_that("Remaining VVM Life is calculated correctly", {
  two_days = seq(ymd_hms("2017-05-06 00:00:10"),
                 ymd_hms("2017-05-08 00:00:00"),
                 by = 60 * 10)
  temps = rep(37, 288)
  df = data.frame(date = two_days,
                  temp = temps)
  df = annotate_vvm(df)
  # kelvin is calculated correctly
  expect_equal(273.15, unique(df$temp_k - df$temp))
  # final cpll2 is greater than 100%
  expect_equal(113.7811, df[nrow(df), ]$cpll2, tolerance = 1e-5)
  # rate constants are correct for given temperature
  expect_equal(0.5689053, unique(df$k2), tolerance = 1e-7)
  expect_equal(0.1646515, unique(df$k7), tolerance = 1e-7)
  expect_equal(0.08232575, unique(df$k14), tolerance = 1e-7)
  expect_equal(0.03847373, unique(df$k30), tolerance = 1e-7)
  # instantaneous life lost is calculated correctly
  expect_equal(0.3950731, unique(df$pll2), tolerance = 1e-7)
  expect_equal(0.1143413, unique(df$pll7), tolerance = 1e-7)
  expect_equal(0.05717066, unique(df$pll14), tolerance = 1e-7)
  expect_equal(0.02671787, unique(df$pll30), tolerance = 1e-7)
})

