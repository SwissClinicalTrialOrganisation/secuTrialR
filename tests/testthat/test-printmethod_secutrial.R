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


sT_export_short <- read_secuTrial_raw(data_dir = short_export_location)
sT_export_long <- read_secuTrial_raw(data_dir = long_export_location)
sT_export_sep_ref <- read_secuTrial_raw(data_dir = sep_ref_export_loc)

p_short <- print(sT_export_short)
d_short <- dim(p_short)

p_long <- print(sT_export_long)
d_long <- dim(p_long)

test_that("number of rows", expect_equal(d_short[1], 13))
test_that("number of columns", expect_equal(d_short[2], 5))
test_that("short equals long", expect_equal(d_short, d_long))

p_sep_ref <- print(sT_export_sep_ref)

test_that("Dimensions", expect_equal(dim(p_sep_ref), c(31, 5)))
test_that("Head of nrow", expect_equal(head(p_sep_ref$nrow), c(4, 8, 8, 29, 85, 3)))
