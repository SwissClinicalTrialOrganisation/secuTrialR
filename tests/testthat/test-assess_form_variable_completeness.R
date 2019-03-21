context("assess variable completeness")

# load exports
short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- load_secuTrial_export(data_dir = short_export_location)
sT_export_long <- load_secuTrial_export(data_dir = long_export_location)

# load validation overview
val_ovv_location <- system.file("extdata", "bmd_validation_overview.xlsx", package = "secuTrialR")
val_ovv <- load_validation_overview(data_dir = val_ovv_location)

# test error handling
test_that("Exceptions correctly triggered.", {
  expect_error(assess_form_variable_completeness())
  expect_error(assess_form_variable_completeness(occ_in_vp = 0))
  expect_error(assess_form_variable_completeness(validation_overview = val_ovv, completeness = "yes"))
})

# bmd completeness allforms short and long
completeness_bmd_short <- assess_form_variable_completeness(form = sT_export_short$bmd,
                                                            patient_table = sT_export_short$patient,
                                                            validation_overview = val_ovv,
                                                            completeness = "allforms",
                                                            occ_in_vp = 5)

completeness_bmd_long <- assess_form_variable_completeness(form = sT_export_long$dem00bmd,
                                                           patient_table = sT_export_long$patient,
                                                           validation_overview = val_ovv,
                                                           completeness = "allforms",
                                                           occ_in_vp = 5)

# test outputs allforms
test_that("Variable completeness allforms working.", {
  expect_equal(completeness_bmd_short, completeness_bmd_long)
  expect_equal(dim(completeness_bmd_short), c(7, 4))
  expect_equal(sum(completeness_bmd_long$timesentered), 3523)
  expect_equal(sum(completeness_bmd_long$timesmissing), 432)
  expect_equal(round(completeness_bmd_long$completeness, digits = 3), c( 0.890, 0.888, 0.892, 0.888, 0.892, 0.892, 0.892))
})

# bmd completeness savedforms short and long
completeness_bmd_short_saved <- assess_form_variable_completeness(form = sT_export_short$bmd,
                                                                  patient_table = sT_export_short$patient,
                                                                  validation_overview = val_ovv,
                                                                  completeness = "savedforms")

completeness_bmd_long_saved <- assess_form_variable_completeness(form = sT_export_long$dem00bmd,
                                                                 patient_table = sT_export_long$patient,
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
