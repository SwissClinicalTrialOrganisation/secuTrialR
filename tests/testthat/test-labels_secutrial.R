context("labels")

skip_on_cran()

short_export_location <- system.file("extdata", "sT_exports", "BMD",
                                     "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata", "sT_exports", "BMD",
                                    "s_export_CSV-xls_BMD_long_en_utf8.zip",
                                    package = "secuTrialR")

sT_export_short <- read_secuTrial_raw(data_dir = short_export_location)
sT_export_long <- read_secuTrial_raw(data_dir = long_export_location)


# labels_secuTrial testing
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

# more tests for labels_secuTrial with more complex CDMA
sT_export <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
                                               "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                               package = "secuTrialR"))

one_label <- labels_secuTrial(sT_export, form = "treatment")
two_labels <- labels_secuTrial(sT_export, form = c("surgeries", "treatment"))
three_labels <- labels_secuTrial(sT_export, form = c("sae", "treatment", "surgeries"))
test_that("Form option working correctly", {
  expect_equal(length(one_label), 1)
  expect_equal(length(two_labels), 3)
  expect_equal(length(three_labels), 17)
  expect_equal(as.vector(two_labels), c("Type", "Organ", "Randomization"))
})

# label_secuTrial testing
test_that("labelling object works",
          expect_warning({
            label_secuTrial(sT_export_short)
            label_secuTrial(sT_export_long)
  }
  , regexp = NA))

sl <- label_secuTrial(sT_export_short)
ll <- label_secuTrial(sT_export_long)

test_that("age label", {
  expect_equal(label(sl$bmd$age), "Age")
  expect_equal(label(ll$dem00bmd$age), "Age")
})


sdat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "snames",
                                         "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                         package = "secuTrialR"))
sdat <- label_secuTrial(sdat)
ldat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
                                         "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
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


# labels get propogated to factors/dates
ldat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
                                          "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                                          package = "secuTrialR"))
ldat <- label_secuTrial(ldat)

fl <- factorize_secuTrial(ldat)
dl <- suppressWarnings(dates_secuTrial(ldat))
dfl <- suppressWarnings(dates_secuTrial(fl))
test_that("label propogated to factor", {
  expect_equal(label(fl$ctu05outcome$follow_up.factor), label(fl$ctu05outcome$follow_up))
})
test_that("label propogated to date(time)", {
  expect_equal(label(dl$ctu05outcome$death_date.date), label(dl$ctu05outcome$death_date))
  expect_equal(label(dl$ctu05baseline$hiv_date.datetime), label(dl$ctu05baseline$hiv_date))
})
test_that("label propogated to factor and date(time)", {
  expect_equal(label(dfl$ctu05outcome$follow_up.factor), label(dfl$ctu05outcome$follow_up))
  expect_equal(label(dfl$ctu05outcome$death_date.date), label(dfl$ctu05outcome$death_date))
  expect_equal(label(dl$ctu05baseline$hiv_date.datetime), label(dl$ctu05baseline$hiv_date))
})
