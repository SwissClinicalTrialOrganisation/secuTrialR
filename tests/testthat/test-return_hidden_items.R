context("return hidden items")

skip_on_cran()

sdat <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                   "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata", "sT_exports", "lnames",
                                  "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                  package = "secuTrialR"))

export_location_only_col_names <- system.file("extdata", "sT_exports", "exp_opt",
                                              "s_export_CSV-xls_CTU05_only_column_names.zip",
                                              package = "secuTrialR")

sT_export_only_col_names <- read_secuTrial_raw(data_dir = export_location_only_col_names)


test_that("Test fail", {
  expect_error(return_hidden_items(1337))
  expect_error(return_hidden_items(sT_export_only_col_names))
})

test_that("Test output", {
  expect_equal(return_hidden_items(sdat), return_hidden_items(ldat))
  expect_equal(nrow(return_hidden_items(ldat)), 0)
})
