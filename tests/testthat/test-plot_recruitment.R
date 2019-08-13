context("plot recruitment")

sdat <- read_secuTrial(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_shortnames.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata",
                                  "s_export_CSV-xls_CTU05_longnames.zip",
                                  package = "secuTrialR"))

test_that("Test fail", {
  expect_error(plot_recruitment(1337))
})

test_that("Test output", {
  expect_equal(plot_recruitment(sdat, return_data = TRUE),
               plot_recruitment(ldat, return_data = TRUE))
  expect_equal(dim(plot_recruitment(ldat, return_data = TRUE)), c(11, 2))
})
