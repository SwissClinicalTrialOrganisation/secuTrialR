context("load secuTrial validation overview")

# load data
val_ovv_location <- system.file("extdata", "bmd_validation_overview.xlsx", package = "secuTrialR")
val_ovv <- load_validation_overview(data_dir = val_ovv_location)

test_that("Validation Overview loaded correctly.", {
  expect_equal(dim(val_ovv), c(5, 12))
  expect_equal(unique(val_ovv$Column), c("bmd", "grouping", "age"))
})
