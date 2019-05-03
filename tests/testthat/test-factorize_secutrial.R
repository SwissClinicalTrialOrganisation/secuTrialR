context("factorize")

short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- load_secuTrial_export(data_dir = short_export_location)
sT_export_long <- load_secuTrial_export(data_dir = long_export_location)

test_that("separate table warning", expect_error(factorize_secuTrial(sT_export_short)))


dat <- load_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                         package = "secuTrialR"))

test_that("error on factorize", expect_warning(factorize_secuTrial(dat), regexp = NA))
test_that("warning on factorize", expect_error(factorize_secuTrial(dat), regexp = NA))

f <- factorize_secuTrial(dat)

unique(dat$cl$column)

sAF <- options()$stringsAsFactors
options(stringsAsFactors = FALSE)

dat <- load_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                         package = "secuTrialR"))
f <- factorize_secuTrial(dat)

w <- sapply(f$ctu05ae, class) == "factor"
names(w)[w]

test_that("number of factors in AE form", expect_equal(sum(w), 8))

options(stringsAsFactors = sAF)

