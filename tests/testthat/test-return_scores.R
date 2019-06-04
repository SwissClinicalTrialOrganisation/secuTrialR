context("return scores")

sdat <- read_secuTrial(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_shortnames.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata",
                                  "s_export_CSV-xls_CTU05_longnames.zip",
                                  package = "secuTrialR"))

test_that("Test fail", {
  expect_error(return_scores(1337))
})

test_that("Test output", {
  expect_equal(return_scores(sdat), return_scores(ldat))
  expect_equal(length(return_scores(ldat)), 3)
})
