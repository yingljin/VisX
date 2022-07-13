
##### corstars #####

test_that("correlation matrix with significance", {
  df <- data.frame(x1 = rnorm(100),
                   x2 = sample(c("a", "b", "c"), size = 100, replace = T),
                   x3 = sample(1:3, size = 100, replace = T))
  df$x4 <- df$x1+rnorm(100, 0, 0.01)
  type <- c("numeric", "factor", "ordinal", "numeric")
  test_cor <- pairwise_cor(df, type)
  test_mat <- corstars(test_cor$cor_value, test_cor$cor_p, type)

  expect_setequal(test_mat$col_id, c("numeric", "factor", "ordinal"))
  expect_setequal(test_mat$row_id, c("numeric", "numeric", "factor"))

})


##### get_r2 #####

test_that("get R-squared", {

  df <- data.frame(x1 = rnorm(100),
                   x2 = sample(c("a", "b", "c"), size = 100, replace = T),
                   x3 = sample(1:3, size = 100, replace = T))
  df$x4 <- df$x1+rnorm(100, 0, 0.01)
  type <- c("numeric", "factor", "ordinal", "numeric")

  test_r2 <- get_r2(df, type)

  expect_length(test_r2, ncol(df))
  expect_gt(test_r2[1], test_r2[2])
  expect_gt(test_r2[4], test_r2[3])


})

