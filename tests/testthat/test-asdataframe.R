context("as.data.frame")

skip_on_cran()

bmd <- read_secuTrial(data_dir =
                           system.file("extdata", "sT_exports", "BMD",
                                       "s_export_CSV-xls_BMD_long_en_utf8.zip",
                                       package = "secuTrialR"))
test_that("bmd", {
  test <- new.env()


  expect_error(as.data.frame(bmd, envir = .GlobalEnv), regexp = NA)
  expect_true(exists("dem00bmd"))
  expect_equal(dem00bmd, bmd$dem00bmd)
  expect_false(any(sapply(c("forms", "casenodes", "centres", "items"), exists)))

  expect_error(as.data.frame(bmd, regex = "em00",
                             envir = .GlobalEnv), regexp = NA)
  expect_equal(dbmd, bmd$dem00bmd)
  expect_true(exists("dbmd"))

  expect_error(as.data.frame(bmd, regex = "m00", meta = TRUE,
                             envir = .GlobalEnv), regexp = NA)
  expect_equal(debmd, bmd$dem00bmd)
  expect_true(exists("debmd"))
  expect_true(all(sapply(c("forms", "casenodes", "centres", "items"), exists)))


  expect_error(as.data.frame(bmd, regex = "dem00", rep = "foo",
                             data.frames = c("dem00bmd"),
                             envir = .GlobalEnv), regexp = NA)
  expect_equal(foobmd, bmd$dem00bmd)
  expect_true(exists("foobmd"))
  expect_false(exists("atmnpfoobmd"))


  expect_error(as.data.frame(bmd, envir = test,
                             data.frames = c("dem00bmd")), regexp = NA)
  expect_true(exists("dem00bmd", envir = test))

})

test_that("errors", {
  expect_error(as.data.frame(bmd, data.frames = "foo", envir = .GlobalEnv),
               regexp = "unrecognised data.frame specified")

  expect_error(as.data.frame(bmd, regex = TRUE, envir = .GlobalEnv),
               regexp = "regex should be either NULL or character")

  expect_warning(as.data.frame(bmd, regex = c("a", "b"), envir = .GlobalEnv),
               regexp = "argument 'pattern' has length > 1")

  expect_error(as.data.frame(bmd, rep = TRUE, envir = .GlobalEnv),
               regexp = "rep should be character")

  expect_warning(as.data.frame(bmd, regex = c("a", "b"), rep = c("c", "d"),
                               envir = .GlobalEnv),
               regexp = "argument 'replacement' has length > 1")

  expect_error(as.data.frame(bmd, envir = TRUE),
               regexp = "envir should be an environment")


})
