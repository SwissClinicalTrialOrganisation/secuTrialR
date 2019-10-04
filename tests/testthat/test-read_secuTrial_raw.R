context("load full secuTrial export testing")

short_export_location <- system.file("extdata", "sT_exports", "BMD",
                                     "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata", "sT_exports", "BMD",
                                    "s_export_CSV-xls_BMD_long_en_utf8.zip",
                                    package = "secuTrialR")

no_add_id_export_location <- system.file("extdata", "sT_exports", "export_options",
                                         "s_export_CSV-xls_CTU05_20191003-144833_no_addid.zip",
                                         package = "secuTrialR")

no_centre_info_export_location <- system.file("extdata", "sT_exports", "export_options",
                                              "s_export_CSV-xls_CTU05_20191003-144655_no_centre_info.zip",
                                              package = "secuTrialR")

all_info_export_location <- system.file("extdata", "sT_exports", "export_options",
                                        "s_export_CSV-xls_CTU05_20191003-144349_all_info.zip",
                                        package = "secuTrialR")


sT_export_short <- read_secuTrial_raw(data_dir = short_export_location)
sT_export_long <- read_secuTrial_raw(data_dir = long_export_location)
sT_export_no_add_id <- read_secuTrial_raw(data_dir = no_add_id_export_location)
sT_export_no_centre_info <- read_secuTrial_raw(data_dir = no_centre_info_export_location)
sT_export_all_info <- read_secuTrial_raw(data_dir = all_info_export_location)

# test length of list
test_that("All data tables loaded.", {
  expect_equal(length(names(sT_export_short)), 14)
  expect_equal(length(names(sT_export_long)), 14)
  expect_equal(length(names(sT_export_no_add_id)), 32)
  # one less because there is no centres table
  expect_equal(length(names(sT_export_no_centre_info)), 31)
  expect_equal(length(names(sT_export_all_info)), 32)
})

# check dimensions of loaded data
test_that("Data dimensions are correct.", {
  # dim CTU05
  # 62 because everything is available
  expect_equal(dim(sT_export_all_info$baseline), c(17, 62))
  # 61 because pat_id column has not been added
  expect_equal(dim(sT_export_no_add_id$baseline), c(17, 61))
  # 61 because centre column has not been added
  expect_equal(dim(sT_export_no_centre_info$baseline), c(17, 61))
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

# check add_id and lab_id
test_that("add_id and lab_id correctly determined", {
  expect_true(sT_export_all_info$export_options$add_id)
  expect_true(sT_export_no_centre_info$export_options$add_id)
  expect_false(sT_export_no_add_id$export_options$add_id)
  expect_true(sT_export_short$export_options$add_id)
  expect_true(sT_export_long$export_options$add_id)
  expect_false(sT_export_short$export_options$lab_id)
  expect_false(sT_export_long$export_options$lab_id)
})

# check fail "Column names"
test_that("No Column names failing", {
  expect_error(read_secuTrial_raw(data_dir = system.file("extdata", "sT_exports", "BMD",
                                     "s_export_CSV-xls_BMD_long_ref_en_utf8.zip",
                                     package = "secuTrialR"))
              )
})
