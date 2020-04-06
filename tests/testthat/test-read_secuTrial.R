context("read_secutrial")

skip_on_cran()

# zipped
l1 <- system.file("extdata", "sT_exports", "lnames", "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
            package = "secuTrialR")

l2 <- system.file("extdata", "sT_exports", "lnames", "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                  package = "secuTrialR")

s1 <- system.file("extdata", "sT_exports", "snames", "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                  package = "secuTrialR")

# zipped
bmd_zipped_loc <- system.file("extdata", "sT_exports", "BMD", "s_export_CSV-xls_BMD_short_en_utf8.zip",
                              package = "secuTrialR")
# unzipped
bmd_unzipped_loc <- system.file("extdata", "sT_exports", "BMD", "s_export_CSV-xls_BMD_short_en_utf8",
                                package = "secuTrialR")


ld1 <- read_secuTrial(l1)
test_that("labels present", {
  expect_equal(label(ld1$ctu05ae), "Adverse Events")
  expect_equal(label(ld1$ctu05ae$ae_action), "Action taken")
  expect_equal(label(ld1$ctu05ae$ae_outcome), "Outcome")
  expect_equal(label(ld1$ctu05ae$ae_outcome.factor), "Outcome")
  expect_equal(label(ld1$ctu05ae$ae_onset_date), "Onset of AE")
  expect_equal(label(ld1$ctu05ae$ae_onset_date.date), "Onset of AE")
  })

test_that("post processing indicators (all true)", {
  expect_true(ld1$export_options$factorized)
  expect_true(ld1$export_options$dated)
  expect_true(ld1$export_options$labelled)
})

ld1_2 <- read_secuTrial(l1, FALSE)
test_that("post processing indicators (no label)", {
  expect_true(ld1_2$export_options$factorized)
  expect_true(ld1_2$export_options$dated)
  expect_false(ld1_2$export_options$labelled)
})

ld1_3 <- read_secuTrial(l1, FALSE, FALSE)
test_that("post processing indicators (no label/factor)", {
  expect_false(ld1_3$export_options$factorized)
  expect_true(ld1_3$export_options$dated)
  expect_false(ld1_3$export_options$labelled)
})

test_that("labels not present", {
  expect_null(label(ld1_2$ctu05ae))
  expect_null(label(ld1_2$ctu05ae$ae_action))
  expect_null(label(ld1_2$ctu05ae$ae_outcome))
  expect_null(label(ld1_2$ctu05ae$ae_outcome.factor))
  expect_null(label(ld1_2$ctu05ae$ae_onset_date))
  expect_null(label(ld1_2$ctu05ae$ae_onset_date.date))
})

# compare zipped and unzipped
bmd_zip <- read_secuTrial(bmd_zipped_loc)
bmd_unzip <- read_secuTrial(bmd_unzipped_loc)

test_that("Zipped and unzipped return the same.", {
  expect_true(all.equal(bmd_zip$cn, bmd_unzip$cn))
  expect_true(all.equal(bmd_zip$ctr, bmd_unzip$ctr))
  expect_true(all.equal(bmd_zip$bmd, bmd_unzip$bmd))
  expect_true(all.equal(bmd_zip$fs, bmd_unzip$fs))
  expect_true(all.equal(bmd_zip$vp, bmd_unzip$vp))
  expect_true(all.equal(bmd_zip$qs, bmd_unzip$qs))
})

# test missing file
test_that("Missing file exception.", {
  expect_error(read_secuTrial("thisisnotafile.zip"))
})
