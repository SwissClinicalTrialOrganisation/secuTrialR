context("export options warning")

skip_on_cran()

path_only_col_names <- system.file("extdata", "sT_exports", "exp_opt",
                                   "s_export_CSV-xls_CTU05_only_column_names.zip",
                                   package = "secuTrialR")
sT_only_col_names <- read_secuTrial_raw(path_only_col_names)

path_all_info <- system.file("extdata", "sT_exports", "exp_opt",
                             "s_export_CSV-xls_CTU05_all_info.zip",
                             package = "secuTrialR")
sT_all_info <- read_secuTrial_raw(path_all_info)

path_unzipped <- system.file("extdata", "sT_exports", "BMD",
                             "s_export_CSV-xls_BMD_short_en_utf8",
                             package = "secuTrialR")
sT_unzipped <- read_secuTrial_raw(path_unzipped)

test_that("error, warning and no warning", {
  expect_error(check_export_options(1337))
  expect_message(check_export_options(sT_unzipped))
  expect_message(check_export_options(sT_only_col_names))
  expect_equal(check_export_options(sT_all_info), NULL)
})
