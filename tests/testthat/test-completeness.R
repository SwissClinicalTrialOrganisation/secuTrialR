context("completeness")

# CTU05
s_ctu05 <- read_secuTrial(system.file("extdata",
                                      "s_export_CSV-xls_CTU05_shortnames.zip",
                                      package = "secuTrialR"))
l_ctu05 <- read_secuTrial(system.file("extdata",
                                      "s_export_CSV-xls_CTU05_longnames.zip",
                                      package = "secuTrialR"))
s_ctu05_sep_ref <- read_secuTrial(system.file("extdata",
                                      "s_export_CSV-xls_CTU05_shortnames_sep_ref.zip",
                                      package = "secuTrialR"))

# BMD
l_bmd <- read_secuTrial(system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR"))
s_bmd <- read_secuTrial(system.file("extdata",
                                    "s_export_CSV-xls_BMD.zip",
                                    package = "secuTrialR"))

# TES05
#tes05 <- read_secuTrial(system.file("extdata",
#                                    "s_export_CSV-xls_TES05.zip",
#                                    package = "secuTrialR"))

test_that("Test fail", {
  expect_error(form_status_counts(1337))
})

# long and short cannot match on form_names, so we just check the data columns
cols <- c("pat_id", "completely_filled", "partly_filled", "empty", "with_warnings", "with_errors")

test_that("Test output equality for different export options", {
  expect_equal(form_status_counts(s_ctu05)[, cols], form_status_counts(l_ctu05)[, cols])
  expect_equal(form_status_counts(l_bmd)[, cols], form_status_counts(s_bmd)[, cols])
  expect_equal(form_status_counts(s_ctu05), form_status_counts(s_ctu05_sep_ref))
  expect_equal(form_status_summary(s_ctu05), form_status_summary(s_ctu05_sep_ref))
})

test_that("Test that partly, completely and empty percentages add up to 1 i.e. 100%", {
  # the vector is made up of ones subtracting one from all of them and summing should always return 0
  expect_equal(sum(rowSums(subset(form_status_summary(s_ctu05),
                                  select = c(partly_filled.percent,
                                             completely_filled.percent,
                                             empty.percent))) - 1),
               0)
})


# TODO add tests with warnings and errors and empty data
