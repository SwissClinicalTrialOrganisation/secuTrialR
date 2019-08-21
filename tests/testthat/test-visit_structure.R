context("visit structure")

test_that("secutrialdata object", expect_error(visit_structure(c(1:3))))



short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- read_secuTrial_export(data_dir = short_export_location)
sT_export_long <- read_secuTrial_export(data_dir = long_export_location)

test_that("failure on bmd data", {
  expect_error(visit_structure(sT_export_short))
  expect_error(visit_structure(sT_export_long))
  })


sdat <- read_secuTrial_export(system.file("extdata",
                                          "s_export_CSV-xls_CTU05_shortnames.zip",
                                          package = "secuTrialR"))
ldat <- read_secuTrial_export(system.file("extdata",
                                          "s_export_CSV-xls_CTU05_longnames.zip",
                                          package = "secuTrialR"))

test_that("failure on CTU05 data", {
  expect_error(visit_structure(sdat))
  expect_error(visit_structure(ldat))
})

dat <- read_secuTrial(system.file("extdata",
                                  "s_export_CSV-xls_TES05_short_ISO-8859-15.zip",
                                  package = "secuTrialR"))

test_that("no fail with TES05", {
          expect_error(visit_structure(dat), regexp = NA)
          expect_error(plot(visit_structure(dat)), regexp = NA)
})

vs <- visit_structure(dat)
test_that("correct dims", {
  expect_equal(dim(vs), c(3,6))
})

test_that("all forms included", {
  expect_true(all(unique(as.character(dat$fs$formname)) %in% rownames(vs)))
  expect_true(all(rownames(vs) %in% unique(as.character(dat$fs$formname))))
})

test_that("all visits included", {
  expect_true(all(unique(as.character(dat$vp$mnpvislabel)) %in% names(vs)))
  expect_true(all(names(vs)[-1] %in% unique(as.character(dat$vp$mnpvislabel))))
})

test_that("colsums, rowsums correct", {
  cs <- colSums(vs[, -1], na.rm = TRUE)
  expect_equal(as.numeric(cs), c(1,1,1,1,2))
  rs <- rowSums(vs[, -1], na.rm = TRUE)
  expect_equal(as.numeric(rs), c(1,4,1))
})
