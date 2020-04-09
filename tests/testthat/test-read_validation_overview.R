context("load secuTrial validation overview")

skip_on_cran()

# load data
val_ovv_location <- system.file("extdata", "sT_exports", "BMD", "bmd_validation_overview.xlsx", package = "secuTrialR")
val_ovv <- read_validation_overview(data_dir = val_ovv_location)

test_that("Validation Overview loaded correctly.", {
  expect_equal(dim(val_ovv), c(5, 12))
  expect_equal(unique(val_ovv$Column), c("bmd", "grouping", "age"))
})

# test exception
not_xlsx <- "a_file.xls"
test_that("xls exception correctly triggered.", {
  expect_error(read_validation_overview(data_dir = not_xlsx))
})
