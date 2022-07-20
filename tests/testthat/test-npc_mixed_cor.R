

test_that("NPC", {

  # wunsets variables after usage
  data(mtcars)

  types <- c("numeric", "nominal", rep("numeric", 5), rep("ordinal", 4))

  test_cor <- pairwise_cor(mtcars, types)
  npc <- npc_mixed_cor(test_cor$cor_value, test_cor$cor_type, test_cor$cor_p,
                       types)

  # expect_snapshot(npc)
})
