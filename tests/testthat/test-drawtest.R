requireNamespace("shinytest", quietly = TRUE)
requireNamespace("testthat", quietly = TRUE)
testthat::test_that("test-drawtest", {
  testthat::skip_on_cran()
  appDir <- system.file("draw", package = "grapesDraw")
  shinytest::expect_pass(shinytest::testApp(appDir, compareImages = FALSE))
})
