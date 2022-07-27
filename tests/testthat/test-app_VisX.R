
#### UI functions #####

test_that("UI function", {
  golem::expect_shinytaglist(ui())
})

#### app launch ####
#
# testthat::test_that(
#   "app launches",{
#     # We're creating a new process that runs the app
#     x <- process$new(
#       "R",
#       c(
#         "-e",
#         # As we are in the tests/testthat dir, we're moving
#         # two steps back before launching the whole package
#         # and we try to launch the app
#         "VisX::VisX()"
#       )
#     )
#     # We leave some time for the app to launch
#     # Configure this according to your need
#     Sys.sleep(5)
#     # We check that the app is alive
#     expect_true(x$is_alive())
#     # We kill it
#     x$kill()
#   }
# )
