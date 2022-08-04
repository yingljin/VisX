#### correlation/association of a pair of variables ####

test_that("A pair of variables", {

  withr::local_package("nnet")

  # data
  data <- data.frame(x = rnorm(10), y = rbinom(10, 1, 0.5))
  type1 <- c("numeric", "factor")
  type2 <-  c("numeric", "ordinal")
  cor1 <- pair_cor(data, type1)
  cor2 <- pair_cor(data, type2)

  # expectation
  ## correlation
  expect_gte(cor1[[1]], 0)
  expect_lte(cor1[[1]], 1)
  expect_gte(cor2[[1]], -1)
  expect_lte(cor2[[1]], 1)
  ## type
  expect_equal(cor1[[2]], "pseudoR2")
  expect_equal(cor2[[2]], "GKgamma")
  ## p values
  expect_gte(cor1[[3]], 0)
  expect_lte(cor1[[3]], 1)
  expect_gte(cor2[[3]], 0)
  expect_lte(cor2[[3]], 1)

  # both numeric variables
  expect_error(pari_cor(data, c("numeric", "numeric")))
})

##### pariwise correlation/association #####

test_that("Data frame", {

  withr::local_package("nnet")

  df <- data.frame(x = rnorm(10),  y = rbinom(10, 1, 0.5),
                      z = rbinom(10, 5, 0.5))
  type_df <- c("numeric", "factor", "ordinal")
  cor_df <- pairwise_cor(df, type_df)

  cor_type <- matrix(c("spearman", "pseudoR2", "GKgamma",
                          "pseudoR2", "pseudoR2",  "pseudoR2",
                          "GKgamma",  "pseudoR2", "GKgamma"),
                        nrow=3, ncol=3)
  colnames(cor_type) <- rownames(cor_type) <- colnames(df)

  # expect_gte(cor_df$cor_value[cor_df$cor_type=="pseudoR2"], 0)
  # expect_gte(cor_df$cor_value[cor_df$cor_type!="pseudoR2"], -1)
  # expect_lte(cor_df$cor_value, 1)
  expect_setequal(cor_df$cor_type[1:3, 1:3], cor_type)

 })
