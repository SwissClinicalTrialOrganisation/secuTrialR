context("visit structure")

test_that("secutrialdata object", expect_error(visit_structure(c(1:3))))



short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- load_secuTrial_export(data_dir = short_export_location)
sT_export_long <- load_secuTrial_export(data_dir = long_export_location)

test_that("failure on bmd data", {
  expect_error(visit_structure(sT_export_short))
  expect_error(visit_structure(sT_export_long))
  })


sdat <- load_secuTrial_export(system.file("extdata",
                                          "s_export_CSV-xls_CTU05_shortnames.zip",
                                          package = "secuTrialR"))
ldat <- load_secuTrial_export(system.file("extdata",
                                          "s_export_CSV-xls_CTU05_longnames.zip",
                                          package = "secuTrialR"))

test_that("failure on CTU05 data", {
  expect_error(visit_structure(sdat))
  expect_error(visit_structure(ldat))
})
