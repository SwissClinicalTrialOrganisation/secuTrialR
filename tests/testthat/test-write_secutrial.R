context("write to other format")

sdat <- read_secuTrial_export(system.file("extdata",
                                          "s_export_CSV-xls_CTU05_shortnames.zip",
                                          package = "secuTrialR"))
ldat <- read_secuTrial_export(system.file("extdata",
                                          "s_export_CSV-xls_CTU05_longnames.zip",
                                          package = "secuTrialR"))
tdir <- tempdir()

test_that("writing", {
  expect_error(write_secuTrial(sdat, path = tdir, format = "dta"), regexp = NA)
  expect_error(write_secuTrial(sdat, path = tdir, format = "sas"), regexp = NA)
  expect_error(write_secuTrial(sdat, path = tdir, format = "sav"), regexp = NA)
  expect_error(write_secuTrial(sdat, path = tdir, format = "xpt"), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "dta"), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "sas"), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "sav"), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "xpt"), regexp = NA)
})

test_that("files exist", {
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".dta"))))
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".sas7bdat"))))
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".sav"))))
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".xpt"))))

  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".dta"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".sas7bdat"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".sav"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".xpt"))))

})
# shell.exec(tdir)
