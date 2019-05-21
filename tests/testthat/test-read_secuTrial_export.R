context("load full secuTrial export testing")

short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sT_export_short <- read_secuTrial_export(data_dir = short_export_location)
sT_export_long <- read_secuTrial_export(data_dir = long_export_location)

# only load meta tables
sT_export_short_tables_none <- read_secuTrial_export(data_dir = short_export_location,
                                                     tables = "none")

# meta and atbmd.xls not bmd.xls
sT_export_short_tables_atbmd <- read_secuTrial_export(data_dir = short_export_location,
                                                     tables = "atbmd.xls")

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
  # tables set to "none"
  expect_equal(length(names(sT_export_short_tables_none)), 12)
  # tables set to "all"
  expect_equal(length(names(sT_export_short)), 14)
  expect_equal(length(names(sT_export_long)), 14)
  # meta and atbmd.xls not bmd.xls
  expect_equal(length(sT_export_short_tables_atbmd), 13)
})
