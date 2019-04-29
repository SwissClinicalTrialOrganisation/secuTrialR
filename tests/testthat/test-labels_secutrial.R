context("labels")

short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- load_secuTrial_export(data_dir = short_export_location)
sT_export_long <- load_secuTrial_export(data_dir = long_export_location)

# test number of labels
test_that("Number of labels is correct.", {
  expect_equal(length(labels_secuTrial(sT_export_short)), 3)
  expect_equal(length(labels_secuTrial(sT_export_long)), 3)
})

test_that("Correct label returned", {
  expect_equal(labels_secuTrial(sT_export_short)[["age"]], "Age")
  expect_equal(labels_secuTrial(sT_export_long)[["age"]], "Age")
})

# test for non-existent form name
# i.e. named character(0)
empty_labels_short <- labels_secuTrial(sT_export_short, form="notaforname")
empty_labels_long <- labels_secuTrial(sT_export_long, form="notaforname")

test_that("Non-existent form", {
  expect_true(is.character(empty_labels_short) && length(empty_labels_short) == 0 && !is.null(attr(empty_labels_short, "name")))
  expect_true(is.character(empty_labels_long) && length(empty_labels_long) == 0 && !is.null(attr(empty_labels_long, "name")))
})

# cannot further test form option without more complex CDMA
