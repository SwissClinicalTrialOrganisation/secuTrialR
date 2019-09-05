context("CDMA linkage plot")

# load data
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


# test exception
test_that("secutrialdata object", {
          expect_error(links_secuTrial(c(1:3)))
})

# test for structure of igraph
# this test is quite crude for now but if there is even a minor
# change from what is expected this should appropriately fail
len_short <- links_secuTrial(sT_export_short, plot = FALSE)
len_long <- links_secuTrial(sT_export_long, plot = FALSE)
len_sep_ref <- links_secuTrial(sT_export_sep_ref, plot = FALSE)

test_that("igraph structure", {
  expect_equal(len_short, len_short)
  expect_equal(len_short, 310)
  expect_equal(len_sep_ref, 1266)
})
