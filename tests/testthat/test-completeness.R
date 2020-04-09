context("completeness")

skip_on_cran()

# CTU05
l_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "lnames",
                                      "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                                      package = "secuTrialR"))
s_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                      "s_export_CSV-xls_CTU05_short_ref_miss_en_utf8.zip",
                                      package = "secuTrialR"))
# polish
s_ctu05_pl <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                         "s_export_CSV-xls_CTU05_short_meta_ref_miss_pl_utf8.zip",
                                         package = "secuTrialR"))

# TES05
# warning can be suppressed (it is expected)
suppressWarnings(
s_tes05_iso <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                          "s_export_CSV-xls_TES05_short_ref_en_iso8859-15.zip",
                                          package = "secuTrialR"))
)
# warning can be suppressed (it is expected)
suppressWarnings(
l_tes05_utf <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                          "s_export_CSV-xls_TES05_long_ref_en_utf8.zip",
                                          package = "secuTrialR"))
)

test_that("Test fail", {
  expect_error(form_status_counts(1337))
  expect_error(form_status_counts(c(1, 3, 3, 7)))
})

# long and short cannot match on form_names, so we just check the data columns
cols_counts <- c("pat_id", "completely_filled", "partly_filled", "empty", "with_warnings", "with_errors")

test_that("Test output equality for different export options", {
  expect_equal(form_status_counts(s_ctu05)[, cols_counts], form_status_counts(l_ctu05)[, cols_counts])
  expect_equal(form_status_counts(s_tes05_iso)[, cols_counts], form_status_counts(l_tes05_utf)[, cols_counts])
  # polish vs. english should be the same
  expect_equal(form_status_counts(s_ctu05_pl)[, cols_counts], form_status_counts(l_ctu05)[, cols_counts])
})

test_that("Test column sums", {
  expect_equal(as.vector(colSums(form_status_counts(l_ctu05)[, 3:7])), c(74, 5, 0, 0, 0))
  expect_equal(as.vector(colSums(form_status_counts(s_tes05_iso)[, 3:7])), c(21, 12, 4, 0, 0))
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
                                               counts_for_custom_tests$form_name == "bl"), ]$completely_filled,
               1)
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-4" &
                                               counts_for_custom_tests$form_name == "fuvisit"), ]$completely_filled,
               3)
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-4" &
                                               counts_for_custom_tests$form_name == "fuvisit"), ]$partly_filled,
               1)
  expect_equal(counts_for_custom_tests[which(counts_for_custom_tests$pat_id == "RPACKRIG-USZ-4" &
                                               counts_for_custom_tests$form_name == "intervals"), ]$completely_filled,
               1)
})

test_that("Test that partly, completely and empty percentages add up to 1 i.e. 100%", {
  # the vector is made up of ones subtracting one from all of them and summing should always return 0
  expect_equal(sum(rowSums(subset(form_status_summary(s_ctu05),
                                  select = c("partly_filled.percent",
                                             "completely_filled.percent",
                                             "empty.percent"))) - 1),
               0)
  expect_equal(sum(rowSums(subset(form_status_summary(l_tes05_utf),
                                  select = c("partly_filled.percent",
                                             "completely_filled.percent",
                                             "empty.percent"))) - 1),
               0)
})

cols_summary <- c("partly_filled", "completely_filled", "empty", "with_warnings",
                  "with_errors", "partly_filled.percent", "completely_filled.percent",
                  "empty.percent", "with_warnings.percent", "with_errors.percent", "form_count")

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
  # polish vs. english should be the same
  expect_equal(colSums(form_status_summary(s_ctu05_pl)[, cols_summary]),
               colSums(form_status_summary(l_ctu05)[, cols_summary]))
})

# TODO add more tests with warnings and errors and empty data

# subset_secuTrial tests for plot_recruitment
# centres
sdat_berlin <- subset_secuTrial(s_ctu05, centre = "Charité Berlin (RPACK)")
sdat_no_berlin <- subset_secuTrial(s_ctu05, centre = "Charité Berlin (RPACK)", exclude = TRUE)

summary_all <- form_status_summary(s_ctu05)
summary_berlin <- form_status_summary(sdat_berlin)
summary_no_berlin <- form_status_summary(sdat_no_berlin)
counts_all <- form_status_counts(s_ctu05)
counts_berlin <- form_status_counts(sdat_berlin)
counts_no_berlin <- form_status_counts(sdat_no_berlin)

count_cols <- c("completely_filled", "partly_filled", "empty", "with_warnings", "with_errors")

test_that("Test output after subsetting centres", {
  expect_equal(summary_all[which(summary_all$form_name == "baseline"), ]$partly_filled,
               (summary_berlin[which(summary_berlin$form_name == "baseline"), ]$partly_filled +
                  summary_no_berlin[which(summary_no_berlin$form_name == "baseline"), ]$partly_filled))
  expect_equal(summary_all[which(summary_all$form_name == "baseline"), ]$form_count,
               (summary_berlin[which(summary_berlin$form_name == "baseline"), ]$form_count +
                  summary_no_berlin[which(summary_no_berlin$form_name == "baseline"), ]$form_count))
  expect_equal(summary_all[which(summary_all$form_name == "baseline"), ]$completely_filled,
               (summary_berlin[which(summary_berlin$form_name == "baseline"), ]$completely_filled +
                  summary_no_berlin[which(summary_no_berlin$form_name == "baseline"), ]$completely_filled))
  expect_equal(nrow(counts_all), (nrow(counts_berlin) + nrow(counts_no_berlin)))
  expect_equal(colSums(counts_all[, count_cols]), (colSums(counts_no_berlin[, count_cols]) +
                                                   colSums(counts_berlin[, count_cols])))
})

# participants
id_set <- c("RPACK-CBE-002", "RPACK-INS-014", "RPACK-USB-123")

l_ctu05_rm <- subset_secuTrial(l_ctu05, participant = id_set, exclude = TRUE)
l_ctu05_keep <- subset_secuTrial(l_ctu05, participant = id_set)

counts_rm_ids <- form_status_counts(l_ctu05_rm)
counts_keep_ids <- form_status_counts(l_ctu05_keep)

test_that("Test output after subsetting participants", {
  expect_equal(colSums(counts_all[, count_cols]), (colSums(counts_rm_ids[, count_cols]) +
                                                   colSums(counts_keep_ids[, count_cols])))
})


# centre and participants
no_bern_no_basel <- subset_secuTrial(s_ctu05,
                                     participant = "RPACK-USB-123", centre = "Inselspital Bern (RPACK)",
                                     exclude = TRUE)

counts_no_bern_no_basel <- form_status_counts(no_bern_no_basel)
summary_no_bern_no_basel <- form_status_summary(no_bern_no_basel)

test_that("Test output after subsetting centres and participants together", {
  expect_equal(counts_no_bern_no_basel, counts_berlin)
  expect_equal(summary_no_bern_no_basel, summary_berlin)
})
