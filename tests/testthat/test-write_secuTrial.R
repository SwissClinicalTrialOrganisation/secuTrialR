context("write to other format")

skip_on_cran()

sdat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "snames",
                                          "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                          package = "secuTrialR"))
ldat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
                                          "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
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

# test file content
write_secuTrial(sdat, format = "dta", path = tdir)
write_secuTrial(sdat, format = "sav", path = tdir)
write_secuTrial(sdat, format = "sas", path = tdir)
write_secuTrial(sdat, format = "xpt", path = tdir)
sdat_dta_bl <- read_dta(paste0(tdir, "/baseline.dta"))
sdat_sav_bl <- read_sav(paste0(tdir, "/baseline.sav"))
sdat_sas_bl <- read_sas(paste0(tdir, "/baseline.sas7bdat"))
sdat_xpt_bl <- read_xpt(paste0(tdir, "/baseline.xpt"))

test_that("Baseline weight data is equal exist", {
  expect_true(all.equal(as.vector(sdat_dta_bl$weight), as.vector(sdat_sav_bl$weight)))
  expect_true(all.equal(as.vector(sdat_dta_bl$weight), as.vector(sdat_sas_bl$weight)))
  expect_true(all.equal(as.vector(sdat_dta_bl$weight), as.vector(sdat_xpt_bl$weight)))
  expect_true(all.equal(as.vector(sdat_dta_bl$weight), as.vector(sdat$baseline$weight)))
  expect_true(all.equal(as.vector(sdat_dta_bl$weight), as.vector(ldat$ctu05baseline$weight)))
})

# test error catch for non-included formats
test_that("Format illegal", {
  expect_error(write_secuTrial(sdat, format = "thatsnotit", path = tdir))
})
