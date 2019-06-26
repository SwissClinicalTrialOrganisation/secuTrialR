context("load full secuTrial export testing")

short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- read_secuTrial_export(data_dir = short_export_location)
sT_export_long <- read_secuTrial_export(data_dir = long_export_location)

# test length of list
test_that("All data tables loaded.", {
  expect_equal(length(names(sT_export_short)), 14)
  expect_equal(length(names(sT_export_long)), 14)
})

# check dimensions of loaded data
test_that("Data dimensions are correct.", {
  # dim bone mineral density
  expect_equal(dim(sT_export_short$bmd), dim(sT_export_long$dem00bmd))
  expect_equal(dim(sT_export_long$dem00bmd), c(504, 27))
  # dim centre
  expect_equal(dim(sT_export_long$centre), dim(sT_export_short$ctr))
  expect_equal(dim(sT_export_short$ctr), c(1, 3))
  # dim casenode
  expect_equal(dim(sT_export_long$casenodes), dim(sT_export_short$cn))
  expect_equal(dim(sT_export_short$cn), c(113, 13))
})

# check amount of loaded tables
test_that("Correct number of tables loaded.", {
  expect_equal(length(names(sT_export_short)), 14)
  expect_equal(length(names(sT_export_long)), 14)
})

# check add_id and lab_id
test_that("add_id and lab_id correctly determined", {
  expect_true(sT_export_short$export_options$add_id)
  expect_true(sT_export_long$export_options$add_id)
  expect_false(sT_export_short$export_options$lab_id)
  expect_false(sT_export_long$export_options$lab_id)
})

# check fail "Column names"
test_that("No Column names failing", {
  expect_error(read_secuTrial_export(data_dir = system.file("extdata",
                                     "s_export_CSV-xls_longnames_ref_val_sep_BMD.zip",
                                     package = "secuTrialR"))
              )
})
