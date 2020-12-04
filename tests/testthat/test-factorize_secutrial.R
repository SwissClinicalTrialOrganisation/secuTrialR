context("factorize")

skip_on_cran()

## lookup table tests
lookup_long_loc <- system.file("extdata", "sT_exports", "lnames",
                               "s_export_CSV-xls_PLAYG_20201203-180930.zip",
                               package = "secuTrialR")

lookup_short_loc <- system.file("extdata", "sT_exports", "snames",
                                "s_export_CSV-xls_PLAYG_20201204-122705.zip",
                                package = "secuTrialR")

test_that("lookup table factorization", {
  # warnings are not related to the lookup but related to incomplete dates
  expect_warning(sT_exp_lookup_long <- read_secuTrial(data_dir = lookup_long_loc))
  expect_warning(sT_exp_lookup_short <- read_secuTrial(data_dir = lookup_short_loc))
  expect_equal(sT_exp_lookup_long$emnpplaygtherapy_sub$therapy_first_lvl.factor,
               sT_exp_lookup_short$etherapy_sub$therapy_first_lvl.factor)
  expect_equal(sT_exp_lookup_long$emnpplaygtherapy_sub$therapy_second_lvl.factor,
               sT_exp_lookup_short$etherapy_sub$therapy_second_lvl.factor)
  expect_equal(levels(sT_exp_lookup_long$emnpplaygtherapy_sub$therapy_first_lvl.factor),
               c("Bacterial infection therapy", "Pain therapy"))
  expect_equal(levels(sT_exp_lookup_long$emnpplaygtherapy_sub$therapy_second_lvl.factor),
               c("Ampicillin", "Paracetamol", "Penicillin", "Ibuprofen", "Flucloxacillin"))
})

# BMD
short_export_location <- system.file("extdata", "sT_exports", "BMD",
                                     "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata", "sT_exports", "BMD",
                                    "s_export_CSV-xls_BMD_long_en_utf8.zip",
                                    package = "secuTrialR")

sT_export_short <- read_secuTrial_raw(data_dir = short_export_location)
sT_export_long <- read_secuTrial_raw(data_dir = long_export_location)

test_that("separate table warning", {
  expect_error(factorize_secuTrial(sT_export_short))
  expect_error(factorize_secuTrial(sT_export_long))
})

# CTU05
ctu05_l <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
                                          "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                                         package = "secuTrialR"))

ctu05_s <- read_secuTrial_raw(system.file("extdata", "sT_exports", "snames",
                                          "s_export_CSV-xls_CTU05_short_ref_miss_en_utf8.zip",
                                           package = "secuTrialR"))

test_that("error on factorize", expect_warning(factorize_secuTrial(ctu05_l), regexp = NA))
test_that("warning on factorize", expect_error(factorize_secuTrial(ctu05_l), regexp = NA))

sAF <- options()$stringsAsFactors
options(stringsAsFactors = FALSE)

fact_ctu05_l <- factorize_secuTrial(ctu05_l)
fact_ctu05_s <- factorize_secuTrial(ctu05_s)
w_l <- sapply(fact_ctu05_l$ctu05ae, class) == "factor"
w_s <- sapply(fact_ctu05_s$ae, class) == "factor"

test_that("number of factors in AE form", {
  expect_equal(sum(w_l), 21)
  expect_equal(sum(w_s), 21)
})

test_that("Levels in liver cirrhosis", {
  expect_equal(levels(fact_ctu05_l$ctu05baseline$liver_cirrh_type.factor), c("C", "B", "A"))
  expect_equal(levels(fact_ctu05_s$baseline$liver_cirrh_type.factor), c("C", "B", "A"))
})

test_that("Levels in follow-up", {
  expect_equal(as.vector(table(fact_ctu05_l$ctu05outcome$follow_up.factor)), c(5, 5, 2))
  expect_equal(levels(fact_ctu05_l$ctu05outcome$follow_up.factor), c("unknown", "ongoing consultation", "death"))
  expect_equal(as.vector(table(fact_ctu05_s$outcome$follow_up.factor)), c(5, 5, 2))
  expect_equal(levels(fact_ctu05_s$outcome$follow_up.factor), c("unknown", "ongoing consultation", "death"))
})

test_that("Levels in SAE", {
  expect_equal(levels(fact_ctu05_l$ctu05sae$sae_drug_relation.factor), c("not assessable",
                                                                         "possible",
                                                                         "unlikely",
                                                                         "probable",
                                                                         "unrelated",
                                                                         "definitely"))
  expect_equal(as.vector(table(fact_ctu05_l$ctu05sae$sae_drug_relation.factor)), c(1, 1, 0, 0, 0, 0))
  expect_equal(levels(fact_ctu05_s$sae$sae_drug_relation.factor), c("not assessable",
                                                                    "possible",
                                                                    "unlikely",
                                                                    "probable",
                                                                    "unrelated",
                                                                    "definitely"))
  expect_equal(as.vector(table(fact_ctu05_s$sae$sae_drug_relation.factor)), c(1, 1, 0, 0, 0, 0))
})

test_that("Levels in meta variables", {
  expect_equal(levels(fact_ctu05_l$ctu05baseline$mnpfcs0.factor), c("empty", "partly filled", "completely filled"))
  expect_equal(as.vector(table(fact_ctu05_l$ctu05baseline$mnpfcs0.factor)), c(0, 3, 14))
  expect_equal(as.vector(table(fact_ctu05_l$ctu05baseline$sigstatus.factor))[1], 17)
  expect_equal(levels(fact_ctu05_s$baseline$mnpfcs0.factor), c("empty", "partly filled", "completely filled"))
  expect_equal(as.vector(table(fact_ctu05_s$baseline$mnpfcs0.factor)), c(0, 3, 14))
  expect_equal(as.vector(table(fact_ctu05_s$baseline$sigstatus.factor))[1], 17)
})

# factorization of reprtitions (subforms)
test_that("Factorization of repetitions working.", {
  expect_true(all_equal(fact_ctu05_s$esurgeries, fact_ctu05_l$emnpctu05surgeries))
})


# warnings for trying to refactorize
test_that("refactorize warning", {
  expect_warning(factorize_secuTrial(fact_ctu05_l))
  expect_warning(factorize_secuTrial(fact_ctu05_s))
})

# manually adding duplicate factor levels to cl table for mnpptnid
reference_line <- as.vector(ctu05_l$cl[158, ])
duplicate_1 <- reference_line
duplicate_2 <- reference_line
duplicate_1$code <- 1987
duplicate_2$code <- 2019
ctu05_l$cl <- rbind(ctu05_l$cl, duplicate_1, duplicate_2)

test_that("Exception for duplicated factor levels in working.", {
  expect_true(factorize_secuTrial(ctu05_l)$export_options$factorized)
})

options(stringsAsFactors = sAF)
