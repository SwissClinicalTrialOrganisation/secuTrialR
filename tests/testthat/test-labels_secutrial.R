context("labels")

short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- load_secuTrial_export(data_dir = short_export_location)
sT_export_long <- load_secuTrial_export(data_dir = long_export_location)


# labels_secuTrial
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
empty_labels_short <- labels_secuTrial(sT_export_short, form = "notaforname")
empty_labels_long <- labels_secuTrial(sT_export_long, form = "notaforname")

test_that("Non-existent form", {
  expect_true(is.character(empty_labels_short) &&
                length(empty_labels_short) == 0 &&
                !is.null(attr(empty_labels_short, "name")))
  expect_true(is.character(empty_labels_long) &&
                length(empty_labels_long) == 0 &&
                !is.null(attr(empty_labels_long, "name")))
})

# cannot further test form option without more complex CDMA

# label_secuTrial

test_that("labelling object works",
          expect_warning({label_secuTrial(sT_export_short)
                          label_secuTrial(sT_export_long)
  }, regexp = NA))

sl <- label_secuTrial(sT_export_short)
ll <- label_secuTrial(sT_export_long)

test_that("age label", {
  expect_equal(label(sl$bmd$age), "Age")
  expect_equal(label(ll$dem00bmd$age), "Age")
})


sdat <- load_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_shortnames.zip",
                                         package = "secuTrialR"))
sdat <- label_secuTrial(sdat)
ldat <- load_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_longnames.zip",
                                         package = "secuTrialR"))
ldat <- label_secuTrial(ldat)
test_that("aspirin label", {
  expect_equal(units(sdat$baseline$aspirin), "Aspirin")
  expect_equal(units(ldat$ctu05baseline$aspirin), "Aspirin")
})



# forms in labels_secuTrial
test_that("multiple forms", {
  expect_equal(length(labels_secuTrial(sdat, c("outcome", "treatment"))), 4)
  expect_equal(length(labels_secuTrial(ldat, c("outcome", "treatment"))), 4)
})
test_that("single form", {
  expect_equal(length(labels_secuTrial(sdat, c("outcome"))), 3)
  expect_equal(length(labels_secuTrial(ldat, c("outcome"))), 3)
})
