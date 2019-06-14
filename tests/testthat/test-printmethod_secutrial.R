context("print method")

short_export_location <- system.file("extdata",
                                     "s_export_CSV-xls_BMD.zip",
                                     package = "secuTrialR")

long_export_location <- system.file("extdata",
                                    "s_export_CSV-xls_longnames_BMD.zip",
                                    package = "secuTrialR")

sep_ref_export_loc <- system.file("extdata",
                                  "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                  package = "secuTrialR")


sT_export_short <- read_secuTrial_export(data_dir = short_export_location)
sT_export_long <- read_secuTrial_export(data_dir = long_export_location)
sT_export_sep_ref <- read_secuTrial_export(data_dir = sep_ref_export_loc)

p_short <- print(sT_export_short, FALSE)
d_short <- dim(p_short)
out_short_f <- capture.output(print(sT_export_short, FALSE))
out_short_t <- capture.output(print(sT_export_short, TRUE))

p_long <- print(sT_export_long, FALSE)
d_long <- dim(p_long)
out_long_f <- capture.output(print(sT_export_long, FALSE))
out_long_t <- capture.output(print(sT_export_long, TRUE))


test_that("number of rows", expect_equal(d_short[1], 13))
test_that("number of columns", expect_equal(d_short[2], 5))
test_that("short equals long", expect_equal(d_short, d_long))

p_sep_ref <- print(sT_export_sep_ref, FALSE)

test_that("Dimensions", expect_equal(dim(p_sep_ref), c(31, 5)))
test_that("Head of nrow", expect_equal(head(p_sep_ref$nrow), c(4, 8, 8, 29, 85, 3)))

test_that("print option", {
  expect_equal(length(out_short_t), length(out_short_f)+1) # should be one element longer
  expect_equal(length(out_long_t), length(out_long_f)+1) # should be one element longer
})
