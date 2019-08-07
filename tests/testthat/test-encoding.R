context("character encoding")

# systematic encoding tests
# functional encodings
# utf8
exp_opt_ctu05_utf8 <- read_export_options(system.file("extdata", "encoding_examples",
                                                      "s_export_CSV-xls_CTU05_20190807-143033_fr_utf8.zip",
                                                      package = "secuTrialR"))
# utf8 + bom
exp_opt_ctu05_utf8bom <- read_export_options(system.file("extdata", "encoding_examples",
                                                         "s_export_CSV-xls_CTU05_20190807-143146_fr_utf8bom.zip",
                                                         package = "secuTrialR"))
# ISO-8859-15
exp_opt_ctu05_iso15 <- read_export_options(system.file("extdata", "encoding_examples",
                                                       "s_export_CSV-xls_CTU05_20190807-143345_fr_ISO-8859-15.zip",
                                                       package = "secuTrialR"))
# ISO-8859-1
exp_opt_ctu05_iso1 <- read_export_options(system.file("extdata", "encoding_examples",
                                                      "s_export_CSV-xls_CTU05_20190807-143436_fr_ISO-8859-1.zip",
                                                      package = "secuTrialR"))

# test encoding parser
test_that("encodings parsed as expected", {
  expect_equal(exp_opt_ctu05_utf8$encoding, "UTF-8")
  expect_equal(exp_opt_ctu05_utf8bom$encoding, "UTF-8 + BOM")
  expect_equal(exp_opt_ctu05_iso15$encoding, "ISO-8859-15")
  expect_equal(exp_opt_ctu05_iso1$encoding, "ISO-8859-1")
})

# test for equality
expected_output <- c("data_dir", "time_of_export", "encoding")

test_that("all outputs are equal", {
  # unequality are expected only in data_dir, time_of_export and encoding
  # all other elements should be exactly equal since the exports were created
  # with all other options equal
  expect_equal(names(which(unlist(exp_opt_ctu05_utf8) != unlist(exp_opt_ctu05_iso1))),
               expected_output)
  expect_equal(names(which(unlist(exp_opt_ctu05_utf8) != unlist(exp_opt_ctu05_iso15))),
               expected_output)
  expect_equal(names(which(unlist(exp_opt_ctu05_utf8) != unlist(exp_opt_ctu05_utf8bom))),
               expected_output)
})






# non functional encodings

# macroman
# the parsed options are incorrect thus we disallow MacRoman
test_that("MacRoman is not supported", {
  expect_error(read_export_options(system.file("extdata", "encoding_examples",
                                               "s_export_CSV-xls_CTU05_20190807-143308_fr_macroman.zip",
                                               package = "secuTrialR"))
  )
})

# utf16
# readLines can not propperly handle utf16
test_that("UTF-16 is not supported", {
  expect_error(read_export_options(system.file("extdata", "encoding_examples",
                                               "s_export_CSV-xls_CTU05_20190807-143224_fr_utf16.zip",
                                               package = "secuTrialR"))
  )
})

