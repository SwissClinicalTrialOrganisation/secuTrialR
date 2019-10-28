context("random cases")

# read data
sdat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
                                         "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                         package = "secuTrialR"))

ldat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "longnames",
                                         "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                         package = "secuTrialR"))

bmd <- read_secuTrial(system.file("extdata", "sT_exports", "BMD",
                                  "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                  package = "secuTrialR"))

only_col_names_export_location <- system.file("extdata", "sT_exports", "export_options",
                                              "s_export_CSV-xls_CTU05_20191004-131608_only_column_names.zip",
                                              package = "secuTrialR")
sT_export_only_col_names <- read_secuTrial_raw(data_dir = only_col_names_export_location)

no_centre_info_export_location <- system.file("extdata", "sT_exports", "export_options",
                                              "s_export_CSV-xls_CTU05_20191003-144655_no_centre_info.zip",
                                              package = "secuTrialR")
sT_export_no_centre_info <- read_secuTrial_raw(data_dir = no_centre_info_export_location)


# prepare random case output
cases_sdat <- return_random_cases(sdat_ctu05, percent = 0.25, seed = 1337, date = "2019-04-02",
                                  centres = c("Inselspital Bern (RPACK)", "Charité Berlin (RPACK)"))$cases
cases_ldat <- return_random_cases(ldat_ctu05, percent = 0.25, seed = 1337, date = "2019-04-02",
                                  centres = c("Inselspital Bern (RPACK)", "Charité Berlin (RPACK)"))$cases

test_that("Test output", {
  expect_equal(cases_sdat, cases_ldat)
  # test date
  expect_true(all(cases_sdat$mnpvisstartdate > ymd("2019-04-02")))
  # test percentage
  expect_equal(nrow(return_random_cases(bmd)$cases), 12)
  expect_equal(nrow(return_random_cases(bmd, percent = 0.23)$cases), 26)
  expect_equal(nrow(return_random_cases(bmd, percent = 0.999)$cases), 113)
  # test errors
  expect_error(return_random_cases("bmd"))
  expect_error(return_random_cases(bmd, percent = 99))
  expect_error(return_random_cases(bmd, percent = -1))
  expect_error(suppressWarnings(return_random_cases(bmd, date = 1999)))
  expect_error(return_random_cases(sT_export_only_col_names))
  expect_error(return_random_cases(sT_export_no_centre_info))
})
