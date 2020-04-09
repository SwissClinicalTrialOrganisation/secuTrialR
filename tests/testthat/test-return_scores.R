context("return scores")

skip_on_cran()

sdat <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                   "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata", "sT_exports", "lnames",
                                  "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                  package = "secuTrialR"))

test_that("Test fail", {
  expect_error(return_scores(1337))
})

test_that("Test output", {
  expect_equal(return_scores(sdat), return_scores(ldat))
  expect_equal(length(return_scores(ldat)), 3)
})
