context("annual recruitment")

sdat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
                                         "s_export_CSV-xls_CTU05_shortnames.zip",
                                         package = "secuTrialR"))
ldat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "longnames",
                                         "s_export_CSV-xls_CTU05_longnames.zip",
                                         package = "secuTrialR"))
bmd <- read_secuTrial(system.file("extdata", "sT_exports", "BMD",
                                  "s_export_CSV-xls_BMD.zip",
                                  package = "secuTrialR"))
tes05 <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                    "s_export_CSV-xls_TES05_long_UTF-8.zip",
                                    package = "secuTrialR"))

test_that("Test fail", {
  expect_error(annual_recruitment(1337))
})

test_that("Test output", {
  expect_equal(annual_recruitment(sdat_ctu05),
               annual_recruitment(ldat_ctu05))
  expect_equal(annual_recruitment(sdat_ctu05),
               annual_recruitment(ldat_ctu05))
  expect_equal(sum(as.numeric(annual_recruitment(sdat_ctu05)$Total[2:4])), 11)
  expect_equal(sum(as.numeric(annual_recruitment(sdat_ctu05)$"2018"[2:4])), 1)
  expect_equal(sum(as.numeric(annual_recruitment(sdat_ctu05)$"2019"[2:4])), 10)
  expect_equal(annual_recruitment(tes05)$Total, "7")
  expect_equal(annual_recruitment(bmd)$Total, "113")
  expect_equal(dim(annual_recruitment(ldat_ctu05)), c(4, 4))
  expect_equal(dim(annual_recruitment(bmd)), c(1, 4))
  expect_equal(dim(annual_recruitment(tes05)), c(1, 4))
})
