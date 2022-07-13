# Automated test for data check
# check if it reports warning for NA/no-variation columns

test_that("data check", {
  # empty column
  df1 <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = NA)
  expect_warning(data_check(df1))
  # identical columns
  df2 <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = 1)
  expect_warning(data_check(df2))
  # identical columns
  df3 <- data.frame(x1 = rnorm(100), x2 = rnorm(100), x3 = 1, x4 = "")
  expect_warning(data_check(df3))

})
