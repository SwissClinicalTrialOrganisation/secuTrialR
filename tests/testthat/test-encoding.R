context("character encoding")

skip_on_cran()

# systematic encoding tests

# functional encodings


# nolint start
# file locations
# utf8
exp_opt_ctu05_utf8_loc <- system.file("extdata", "sT_exports", "encodings",
                                      "s_export_CSV-xls_CTU05_short_ref_miss_fr_utf8.zip",
                                      package = "secuTrialR")
# ISO-8859-15
exp_opt_ctu05_iso15_loc <- system.file("extdata", "sT_exports", "encodings",
                                       "s_export_CSV-xls_CTU05_short_ref_miss_fr_iso8859-15.zip",
                                       package = "secuTrialR")
# ISO-8859-1
exp_opt_ctu05_iso1_loc <- system.file("extdata", "sT_exports", "encodings",
                                      "s_export_CSV-xls_CTU05_short_ref_miss_fr_iso8859-1.zip",
                                      package = "secuTrialR")
# export options
# utf8
exp_opt_ctu05_utf8_eo <- read_export_options(exp_opt_ctu05_utf8_loc)
# ISO-8859-15
exp_opt_ctu05_iso15_eo <- read_export_options(exp_opt_ctu05_iso15_loc)
# ISO-8859-1
exp_opt_ctu05_iso1_eo <- read_export_options(exp_opt_ctu05_iso1_loc)

# test encoding parser
test_that("encodings parsed as expected", {
  expect_equal(exp_opt_ctu05_utf8_eo$encoding, "UTF-8")
  expect_equal(exp_opt_ctu05_iso15_eo$encoding, "ISO-8859-15")
  expect_equal(exp_opt_ctu05_iso1_eo$encoding, "ISO-8859-1")
})

# test for equality
expected_output <- c("data_dir", "time_of_export", "encoding")

test_that("all export options outputs are equal", {
  # unequality are expected only in data_dir, time_of_export and encoding
  # all other elements should be exactly equal since the exports were created
  # with all other options equal
  expect_equal(names(which(unlist(exp_opt_ctu05_utf8_eo) != unlist(exp_opt_ctu05_iso1_eo))),
               expected_output)
  expect_equal(names(which(unlist(exp_opt_ctu05_utf8_eo) != unlist(exp_opt_ctu05_iso15_eo))),
               expected_output)
})

# read_secuTrial_raw
# utf8
exp_opt_ctu05_utf8_export <- read_secuTrial_raw(exp_opt_ctu05_utf8_loc)
# ISO-8859-15
exp_opt_ctu05_iso15_export <- read_secuTrial_raw(exp_opt_ctu05_iso15_loc)
# ISO-8859-1
exp_opt_ctu05_iso1_export <- read_secuTrial_raw(exp_opt_ctu05_iso1_loc)

# nolint end

# test for table equality
test_that("table outputs are equal", {
  # differences expected in export_options so export_options (index 1) are removed for this test
  expect_equal(exp_opt_ctu05_utf8_export[-1], exp_opt_ctu05_iso15_export[-1])
  expect_equal(exp_opt_ctu05_utf8_export[-1], exp_opt_ctu05_iso1_export[-1])
})


# non functional encodings

# utf8 + bom (because of unz() function)
test_that("UTF-8 + BOM is not supported", {
  expect_error(read_export_options(system.file("extdata", "sT_exports", "encodings",
                                               "s_export_CSV-xls_CTU05_short_ref_miss_fr_utf8bom.zip",
                                                package = "secuTrialR"))
  )
})

# macroman
# the parsed options are incorrect thus we disallow MacRoman
test_that("MacRoman is not supported", {
  expect_error(read_export_options(system.file("extdata", "sT_exports", "encodings",
                                               "s_export_CSV-xls_CTU05_short_ref_miss_fr_macroman.zip",
                                               package = "secuTrialR"))
  )
})

# utf16
# readLines can not propperly handle utf16
test_that("UTF-16 is not supported", {
  expect_error(read_export_options(system.file("extdata", "sT_exports", "encodings",
                                               "s_export_CSV-xls_CTU05_short_ref_miss_fr_utf16.zip",
                                               package = "secuTrialR"))
  )
})
