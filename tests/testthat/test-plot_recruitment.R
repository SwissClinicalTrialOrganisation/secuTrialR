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
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = FALSE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = FALSE))
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = TRUE))
  expect_equal(dim(plot_recruitment(ldat, return_data = TRUE, show_centres = FALSE)[[1]]), c(11, 4))
  # [[1]] has all data, row count in all other entries summed up should be equal to it
  expect_equal(nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[1]]),
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[2]]) +
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[3]]) +
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[4]])
               )
})
