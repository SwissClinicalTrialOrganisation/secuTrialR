context("factorize")

# BMD
short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- read_secuTrial_export(data_dir = short_export_location)
sT_export_long <- read_secuTrial_export(data_dir = long_export_location)

test_that("separate table warning", expect_error(factorize_secuTrial(sT_export_short)))

# CTU05
dat <- read_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                         package = "secuTrialR"))

test_that("error on factorize", expect_warning(factorize_secuTrial(dat), regexp = NA))
test_that("warning on factorize", expect_error(factorize_secuTrial(dat), regexp = NA))

sAF <- options()$stringsAsFactors
options(stringsAsFactors = FALSE)

dat <- read_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                         package = "secuTrialR"))
fact_dat <- factorize_secuTrial(dat)
w <- sapply(fact_dat$ctu05ae, class) == "factor"

test_that("number of factors in AE form", expect_equal(sum(w), 21))

test_that("Levels in liver cirrhosis", {
  expect_equal(levels(fact_dat$ctu05baseline$liver_cirrh_type.factor), c("C", "B", "A"))
})

test_that("Levels in follow-up", {
  expect_equal(as.vector(table(fact_dat$ctu05outcome$follow_up.factor)), c(5, 5, 2))
  expect_equal(levels(fact_dat$ctu05outcome$follow_up.factor), c("unknown", "ongoing consultation", "death"))
})

test_that("Levels in SAE", {
  expect_equal(levels(fact_dat$ctu05sae$sae_drug_relation.factor), c("not assessable",
                                                              "possible",
                                                              "unlikely",
                                                              "probable",
                                                              "unrelated",
                                                              "definitely"))
  expect_equal(as.vector(table(fact_dat$ctu05sae$sae_drug_relation.factor)), c(1, 1, 0, 0, 0, 0))
})

test_that("Levels in meta variables", {
  expect_equal(levels(fact_dat$ctu05baseline$mnpfcs0.factor), c("empty", "partly filled", "completely filled"))
  expect_equal(as.vector(table(fact_dat$ctu05baseline$mnpfcs0.factor)), c(0, 3, 14))
  expect_equal(as.vector(table(fact_dat$ctu05baseline$sigstatus.factor)), c(17, rep(0, 28)))
})

# warnings for trying to refactorize
test_that("refactorize warning", {
          expect_warning(factorize_secuTrial(fact_dat))
})

options(stringsAsFactors = sAF)
