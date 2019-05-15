context("load_secutrial")



l1 <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
            package = "secuTrialR")

l2 <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames.zip",
                  package = "secuTrialR")

s1 <- system.file("extdata", "s_export_CSV-xls_CTU05_shortnames.zip",
                  package = "secuTrialR")

test_that("sep ref error", {
  expect_error(load_secuTrial(l2))
  expect_error(load_secuTrial(s1))
})

ld1 <- load_secuTrial(l1)
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

ld1_2 <- load_secuTrial(l1, FALSE)
test_that("post processing indicators (no label)", {
  expect_true(ld1_2$export_options$factorized)
  expect_true(ld1_2$export_options$dated)
  expect_false(ld1_2$export_options$labelled)
})

ld1_3 <- load_secuTrial(l1, FALSE, FALSE)
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
