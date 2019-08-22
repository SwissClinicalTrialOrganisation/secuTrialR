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

test_that("errors", {
  expect_error(as.data.frame(bmd, data.frames = "foo"),
               regexp = "unrecognised data.frame specified")

  expect_error(as.data.frame(bmd, regex = TRUE),
               regexp = "regex should be either NULL or character")

  expect_warning(as.data.frame(bmd, regex = c("a", "b")),
               regexp = "argument 'pattern' has length > 1")

  expect_error(as.data.frame(bmd, rep = TRUE),
               regexp = "rep should be character")

  expect_warning(as.data.frame(bmd, regex = c("a", "b"), rep = c("c", "d")),
               regexp = "argument 'replacement' has length > 1")

  expect_error(as.data.frame(bmd, envir = TRUE),
               regexp = "envir should be an environment")


})
