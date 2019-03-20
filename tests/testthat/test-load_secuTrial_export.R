context("load full secuTrial export testing")

short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- load_secuTrial_export(data_dir = short_export_location)
sT_export_long <- load_secuTrial_export(data_dir = long_export_location)

# test length of list
test_that("All data tables loaded.", {
  expect_equal(length(names(sT_export_short)), 14)
  expect_equal(length(names(sT_export_long)), 14)
})

# check dimensions of loaded data
test_that("Data dimensions are correct.", {
  # dim bone mineral density
  expect_equal(dim(sT_export_short$bmd), dim(sT_export_long$dem00bmd))
  expect_equal(dim(sT_export_long$dem00bmd), c(504, 26))
  # dim centre
  expect_equal(dim(sT_export_long$centre), dim(sT_export_short$centre))
  expect_equal(dim(sT_export_short$centre), c(1, 3))
  # dim patient
  expect_equal(dim(sT_export_long$patient), dim(sT_export_short$patient))
  expect_equal(dim(sT_export_short$patient), c(113, 14))
})
