context("assess variable completeness")

skip_on_cran()

# load exports
short_export_location <- system.file("extdata", "sT_exports", "BMD",
                                     "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata", "sT_exports", "BMD",
                                    "s_export_CSV-xls_BMD_long_en_utf8.zip",
                                    package = "secuTrialR")

sT_export_short <- read_secuTrial_raw(data_dir = short_export_location)
sT_export_long <- read_secuTrial_raw(data_dir = long_export_location)

# load validation overview
val_ovv_location <- system.file("extdata", "sT_exports", "BMD",
                                "bmd_validation_overview.xlsx", package = "secuTrialR")
val_ovv <- read_validation_overview(data_dir = val_ovv_location)
val_ovv_no_compl_stat <- val_ovv[, -7]
val_ovv_empty <- val_ovv[0, ]

# test error handling
test_that("Exceptions correctly triggered.", {
  expect_error(assess_form_variable_completeness())
  expect_error(assess_form_variable_completeness(form = sT_export_short$bmd,
                                                 validation_overview = val_ovv,
                                                 occ_in_vp = 0))
  expect_error(assess_form_variable_completeness(form = sT_export_short$bmd,
                                                 validation_overview = val_ovv,
                                                 completeness = "yes"))
  expect_error(assess_form_variable_completeness(form = sT_export_short$bmd,
                                                 casenodes_table = sT_export_short$cn,
                                                 validation_overview = val_ovv,
                                                 occ_in_vp = 1))
  expect_error(assess_form_variable_completeness(form = sT_export_short$bmd,
                                                 casenodes_table = sT_export_short$cn,
                                                 validation_overview = val_ovv_no_compl_stat,
                                                 occ_in_vp = 5))
  expect_warning(assess_form_variable_completeness(form = sT_export_short$bmd,
                                                   casenodes_table = sT_export_short$cn,
                                                   validation_overview = val_ovv,
                                                   occ_in_vp = 5,
                                                   completeness = "savedforms"))
  expect_equal(assess_form_variable_completeness(form = sT_export_short$bmd,
                                                 casenodes_table = sT_export_short$cn,
                                                 validation_overview = val_ovv_empty,
                                                 completeness = "savedforms"),
               "100% completeness. Your Validation overview is empty.")
})

# bmd completeness allforms short and long
completeness_bmd_short <- assess_form_variable_completeness(form = sT_export_short$bmd,
                                                            casenodes_table = sT_export_short$cn,
                                                            validation_overview = val_ovv,
                                                            completeness = "allforms",
                                                            occ_in_vp = 5)

completeness_bmd_long <- assess_form_variable_completeness(form = sT_export_long$dem00bmd,
                                                           casenodes_table = sT_export_long$casenodes,
                                                           validation_overview = val_ovv,
                                                           completeness = "allforms",
                                                           occ_in_vp = 5)

# test outputs allforms
test_that("Variable completeness allforms working.", {
  expect_equal(completeness_bmd_short, completeness_bmd_long)
  expect_equal(dim(completeness_bmd_short), c(3, 4))
  expect_equal(sum(completeness_bmd_long$timesentered), 1507)
  expect_equal(sum(completeness_bmd_long$timesmissing), 188)
  expect_equal(round(completeness_bmd_long$completeness, digits = 3), c(0.890, 0.888, 0.888))
})

# bmd completeness savedforms short and long
completeness_bmd_short_saved <- assess_form_variable_completeness(form = sT_export_short$bmd,
                                                                  casenodes_table = sT_export_short$cn,
                                                                  validation_overview = val_ovv,
                                                                  completeness = "savedforms")

completeness_bmd_long_saved <- assess_form_variable_completeness(form = sT_export_long$dem00bmd,
                                                                 casenodes_table = sT_export_long$cn,
                                                                 validation_overview = val_ovv,
                                                                 completeness = "savedforms")

# test output savedforms
test_that("", {
  expect_equal(completeness_bmd_short_saved, completeness_bmd_long_saved)
  expect_equal(dim(completeness_bmd_short_saved), c(3, 4))
  expect_equal(sum(completeness_bmd_short_saved$timesentered), 1507)
  expect_equal(sum(completeness_bmd_short_saved$timesmissing), 5)
  expect_equal(round(completeness_bmd_short_saved$completeness, digits = 3), c(0.998, 0.996, 0.996))
})
