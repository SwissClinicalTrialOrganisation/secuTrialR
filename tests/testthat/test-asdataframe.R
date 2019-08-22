context("as.data.frame")


bmd <- read_secuTrial(data_dir =
                           system.file("extdata",
                                       "s_export_CSV-xls_longnames_BMD.zip",
                                       package = "secuTrialR"))
test_that("bmd", {
  test <- new.env()


  expect_error(as.data.frame(bmd, meta = FALSE), regexp = NA)
  expect_true(exists("dem00bmd"))
  expect_false(any(sapply(c("forms", "casenodes", "centres", "items"), exists)))

  expect_error(as.data.frame(bmd, meta = FALSE, regex = "em00"), regexp = NA)
  expect_true(exists("dbmd"))

  expect_error(as.data.frame(bmd, regex = "m00"), regexp = NA)
  expect_true(exists("debmd"))
  expect_true(all(sapply(c("forms", "casenodes", "centres", "items"), exists)))


  expect_error(as.data.frame(bmd, regex = "dem00", rep = "foo",
                             data.frames = c("dem00bmd")), regexp = NA)
  expect_true(exists("foobmd"))
  expect_false(exists("atmnpfoobmd"))


  expect_error(as.data.frame(bmd, meta = FALSE, envir = test,
                             data.frames = c("dem00bmd")), regexp = NA)
  expect_true(exists("dem00bmd", envir = test))

})

