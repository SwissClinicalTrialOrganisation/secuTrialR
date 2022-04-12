

# load data
short_export_location <- system.file("extdata", "sT_exports", "BMD",
                                     "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata", "sT_exports", "BMD",
                                    "s_export_CSV-xls_BMD_long_en_utf8.zip",
                                    package = "secuTrialR")

sep_ref_export_loc <- system.file("extdata", "sT_exports", "lnames",
                                  "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                                  package = "secuTrialR")

sT_export_short <- read_secuTrial_raw(data_dir = short_export_location)
sT_export_long <- read_secuTrial_raw(data_dir = long_export_location)
sT_export_sep_ref <- read_secuTrial_raw(data_dir = sep_ref_export_loc)

test_that("correct errors", {
  expect_error(dictionary_secuTrial(c(1:3)))
  expect_error(dictionary_secuTrial(sT_export_short), NA)
  expect_error(dictionary_secuTrial(sT_export_long), NA)
  expect_error(dictionary_secuTrial(sT_export_sep_ref), NA)
})
