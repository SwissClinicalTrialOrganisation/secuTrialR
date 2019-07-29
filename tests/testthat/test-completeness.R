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
cols_counts <- c("pat_id", "completely filled", "partly filled", "empty", "with warnings", "with errors")

test_that("Test output equality for different export options", {
  expect_equal(form_status_counts(s_ctu05)[, cols_counts], form_status_counts(l_ctu05)[, cols_counts])
  expect_equal(form_status_counts(s_tes05_iso)[, cols_counts], form_status_counts(l_tes05_utf)[, cols_counts])
})

test_that("Test column sums", {
  expect_equal(as.vector(colSums(form_status_counts(l_ctu05)[,3:7])), c(74, 5, 0, 0, 0))
  expect_equal(as.vector(colSums(form_status_counts(s_tes05_iso)[,3:7])), c(21, 12, 4, 0, 0))
})

# custom count checks
# as manually compared to the secuTrial web interface
counts_for_custom_tests <- form_status_counts(s_tes05_iso)
test_that("Individual entries", {
  # RPACKRIG-USZ-11111 has 4 (1x baseline, 3x fu visit) empty forms and nothing is filled at all
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-11111" &
                                  counts_for_custom_tests$form_name == "bl"), ]$empty,
               1)
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-11111" &
                                             counts_for_custom_tests$form_name == "fuvisit"), ]$empty,
               3)
  # RPACKRIG-USZ-4 has 1x baseline completely filled,
  #                    3x fu visit completely filled,
  #                    1x fu visit partly filled,
  #                    1x intervals completely filled
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-4" &
                                               counts_for_custom_tests$form_name == "bl"), ]$`completely filled`,
               1)
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-4" &
                                               counts_for_custom_tests$form_name == "fuvisit"), ]$`completely filled`,
               3)
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-4" &
                                               counts_for_custom_tests$form_name == "fuvisit"), ]$`partly filled`,
               1)
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-4" &
                                               counts_for_custom_tests$form_name == "intervals"), ]$`completely filled`,
               1)
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

cols_summary <- c("partly filled", "completely filled", "empty", "with warnings",
                  "with errors", "partly filled.percent", "completely filled.percent",
                  "empty.percent", "with warnings.percent", "with errors.percent", "form_count")

test_that("Test column sums", {
  expect_equal(colSums(form_status_summary(s_ctu05)[, cols_summary]),
               colSums(form_status_summary(l_ctu05)[, cols_summary]))
  expect_equal(round(as.vector(colSums(form_status_summary(l_ctu05)[, cols_summary])), digits = 4),
               c(5, 74, 0, 0, 0, 0.3122, 9.6878, 0, 0, 0, 79))
  expect_equal(colSums(form_status_summary(s_tes05_iso)[, cols_summary]),
               colSums(form_status_summary(l_tes05_utf)[, cols_summary]))
  expect_equal(round(as.vector(colSums(form_status_summary(s_tes05_iso)[, cols_summary])), digits = 4),
               c(12, 21, 4, 0, 0, 2.9774, 2.6798, 0.3429, 0, 0, 37)
               )
})

# TODO add more tests with warnings and errors and empty data
