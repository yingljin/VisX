

test_that("NPC", {

  # wunsets variables after usage
  data(mtcars)

  types <- c("numeric", "nominal", rep("numeric", 5), rep("ordinal", 4))

  test_cor <- expect_silent(pairwise_cor(mtcars, types))
  npc <- expect_silent(npc_mixed_cor(test_cor))

})
