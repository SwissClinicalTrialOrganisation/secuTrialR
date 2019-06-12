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
  expect_false(file.exists(file.path(tdir, paste0("cn", ".dta"))))
  expect_false(file.exists(file.path(tdir, paste0("cn", ".sas7bdat"))))
  expect_false(file.exists(file.path(tdir, paste0("cn", ".sav"))))
  expect_false(file.exists(file.path(tdir, paste0("cn", ".xpt"))))

  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".dta"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".sas7bdat"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".sav"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".xpt"))))
  expect_false(file.exists(file.path(tdir, paste0("casenodes", ".dta"))))
  expect_false(file.exists(file.path(tdir, paste0("casenodes", ".sas7bdat"))))
  expect_false(file.exists(file.path(tdir, paste0("casenodes", ".sav"))))
  expect_false(file.exists(file.path(tdir, paste0("casenodes", ".xpt"))))
})

test_that("writing meta", {
  expect_error(write_secuTrial(sdat, path = tdir, format = "dta", metadata = TRUE), regexp = NA)
  expect_error(write_secuTrial(sdat, path = tdir, format = "sas", metadata = TRUE), regexp = NA)
  expect_error(write_secuTrial(sdat, path = tdir, format = "sav", metadata = TRUE), regexp = NA)
  expect_error(write_secuTrial(sdat, path = tdir, format = "xpt", metadata = TRUE), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "dta", metadata = TRUE), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "sas", metadata = TRUE), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "sav", metadata = TRUE), regexp = NA)
  expect_error(write_secuTrial(ldat, path = tdir, format = "xpt", metadata = TRUE), regexp = NA)
})

test_that("files exist", {
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".dta"))))
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".sas7bdat"))))
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".sav"))))
  expect_true(file.exists(file.path(tdir, paste0("treatment", ".xpt"))))
  expect_true(file.exists(file.path(tdir, paste0("cn", ".dta"))))
  expect_true(file.exists(file.path(tdir, paste0("cn", ".sas7bdat"))))
  expect_true(file.exists(file.path(tdir, paste0("cn", ".sav"))))
  expect_true(file.exists(file.path(tdir, paste0("cn", ".xpt"))))

  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".dta"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".sas7bdat"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".sav"))))
  expect_true(file.exists(file.path(tdir, paste0("ctu05treatment", ".xpt"))))
  expect_true(file.exists(file.path(tdir, paste0("casenodes", ".dta"))))
  expect_true(file.exists(file.path(tdir, paste0("casenodes", ".sas7bdat"))))
  expect_true(file.exists(file.path(tdir, paste0("casenodes", ".sav"))))
  expect_true(file.exists(file.path(tdir, paste0("casenodes", ".xpt"))))
})
