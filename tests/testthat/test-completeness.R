context("completeness")

# CTU05
l_ctu05 <- read_secuTrial(system.file("extdata",
                                      "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                      package = "secuTrialR"))
s_ctu05 <- read_secuTrial(system.file("extdata",
                                      "s_export_CSV-xls_CTU05_shortnames_sep_ref.zip",
                                      package = "secuTrialR"))

# TES05
s_tes05_iso <- read_secuTrial(system.file("extdata",
                                          "s_export_CSV-xls_TES05_short_ISO-8859-15.zip",
                                          package = "secuTrialR"))
l_tes05_utf <- read_secuTrial(system.file("extdata",
                                          "s_export_CSV-xls_TES05_long_UTF-8.zip",
                                          package = "secuTrialR"))


test_that("Test fail", {
  expect_error(form_status_counts(1337))
  expect_error(form_status_counts(c(1, 3, 3, 7)))
})

# long and short cannot match on form_names, so we just check the data columns
cols <- c("pat_id", "completely filled", "partly filled", "empty", "with warnings", "with errors")

test_that("Test output equality for different export options", {
  expect_equal(form_status_counts(s_ctu05)[, cols], form_status_counts(l_ctu05)[, cols])
  expect_equal(form_status_counts(s_tes05_iso)[, cols], form_status_counts(l_tes05_utf)[, cols])
})

test_that("Test that partly, completely and empty percentages add up to 1 i.e. 100%", {
  # the vector is made up of ones subtracting one from all of them and summing should always return 0
  expect_equal(sum(rowSums(subset(form_status_summary(s_ctu05),
                                  select = c("partly filled.percent",
                                             "completely filled.percent",
                                             "empty.percent"))) - 1),
               0)
  expect_equal(sum(rowSums(subset(form_status_summary(l_tes05_utf),
                                  select = c("partly filled.percent",
                                             "completely filled.percent",
                                             "empty.percent"))) - 1),
               0)
})


# TODO add tests with warnings and errors and empty data
