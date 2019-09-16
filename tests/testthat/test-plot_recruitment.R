context("plot recruitment")

sdat <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
                                   "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata", "sT_exports", "longnames",
                                  "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                  package = "secuTrialR"))

ctu05_utf8_french <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                                "s_export_CSV-xls_CTU05_short_ref_miss_fr_utf8.zip",
                                                package = "secuTrialR"))
ctu05_iso15_french <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                                 "s_export_CSV-xls_CTU05_short_ref_miss_fr_iso8859-15.zip",
                                                 package = "secuTrialR"))

test_that("Test fail", {
  expect_error(plot_recruitment(1337))
})

test_that("Test output", {
  # show_centres = FALSE
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = FALSE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = FALSE))
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = FALSE),
               plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = FALSE))
  expect_equal(plot_recruitment(ctu05_utf8_french, return_data = TRUE, show_centres = FALSE),
               plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = FALSE))
  # show_centres = TRUE
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = TRUE))
  expect_equal(plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = TRUE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = TRUE))
  expect_equal(plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = TRUE),
               plot_recruitment(ctu05_utf8_french, return_data = TRUE, show_centres = TRUE))
  expect_equal(dim(plot_recruitment(ldat, return_data = TRUE, show_centres = FALSE)[[1]]), c(11, 4))
  # [[1]] has all data, row count in all other entries summed up should be equal to it
  expect_equal(nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[1]]),
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[2]]) +
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[3]]) +
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[4]])
               )
})
