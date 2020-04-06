context("random participants")

skip_on_cran()

# read data
sdat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                         "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                         package = "secuTrialR"))

ldat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "lnames",
                                         "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                         package = "secuTrialR"))

bmd <- read_secuTrial(system.file("extdata", "sT_exports", "BMD",
                                  "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                  package = "secuTrialR"))

only_col_names_export_location <- system.file("extdata", "sT_exports", "exp_opt",
                                              "s_export_CSV-xls_CTU05_only_column_names.zip",
                                              package = "secuTrialR")
sT_export_only_col_names <- read_secuTrial_raw(data_dir = only_col_names_export_location)

no_centre_info_export_location <- system.file("extdata", "sT_exports", "exp_opt",
                                              "s_export_CSV-xls_CTU05_no_centre_info.zip",
                                              package = "secuTrialR")
sT_export_no_centre_info <- read_secuTrial_raw(data_dir = no_centre_info_export_location)


# prepare random participants output
participants_sdat <- return_random_participants(sdat_ctu05, percent = 0.25, seed = 1337, date = "2019-04-02",
                                                centres = c("Inselspital Bern (RPACK)",
                                                            "Charité Berlin (RPACK)"))$participants
participants_ldat <- return_random_participants(ldat_ctu05, percent = 0.25, seed = 1337, date = "2019-04-02",
                                                centres = c("Inselspital Bern (RPACK)",
                                                            "Charité Berlin (RPACK)"))$participants

test_that("Test output", {
  expect_equal(participants_sdat, participants_ldat)
  # test date
  expect_true(all(participants_sdat$mnpvisstartdate > ymd("2019-04-02")))
  # test percentage
  expect_equal(nrow(return_random_participants(bmd)$participants), 12)
  expect_equal(nrow(return_random_participants(bmd, percent = 0.23)$participants), 26)
  expect_equal(nrow(return_random_participants(bmd, percent = 0.999)$participants), 113)
  # test errors
  expect_error(return_random_participants("bmd"))
  expect_error(return_random_participants(bmd, percent = 99))
  expect_error(return_random_participants(bmd, percent = -1))
  expect_error(suppressWarnings(return_random_participants(bmd, date = 1999)))
  expect_error(return_random_participants(sT_export_only_col_names))
  expect_error(return_random_participants(sT_export_no_centre_info))
  expect_error(return_random_participants(bmd, centres = "Not a centre"))
  expect_error(return_random_participants(bmd, centres = c("Not a centre", "Also not a centre")))
})
