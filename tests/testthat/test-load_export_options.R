context("load export options testing")

# shorten table names
export_options_regular_short <- load_export_options(data_dir = system.file("extdata",
                                                                           "s_export_CSV-xls_BMD.zip",
                                                                           package = "secuTrialR"))
# long table names
export_options_regular_long <- load_export_options(data_dir = system.file("extdata",
                                                                          "s_export_CSV-xls_longnames_BMD.zip",
                                                                          package = "secuTrialR"))
# rectangular shorten table names
export_options_rect_short <- load_export_options(data_dir = system.file("extdata",
                                                                           "s_export_rt-CSV-xls_BMD.zip",
                                                                           package = "secuTrialR"))
# rectangular long table names
export_options_rect_long <- load_export_options(data_dir = system.file("extdata",
                                                                          "s_export_rt-CSV-xls_longnames_BMD.zip",
                                                                          package = "secuTrialR"))

# test shortened table names
test_that("Shorten names identified.", {
  expect_true(export_options_regular_short$short_names)
  expect_false(export_options_regular_long$short_names)
  expect_true(export_options_rect_short$short_names)
  expect_false(export_options_rect_long$short_names)
})

# test zip
test_that("zip archive ending identified.", {
  expect_true(export_options_regular_short$is_zip)
  expect_true(export_options_regular_long$is_zip)
  expect_true(export_options_rect_short$is_zip)
  expect_true(export_options_rect_long$is_zip)
})

# test rectangular identification
test_that("Rectangular/regular export identified.", {
  expect_true(export_options_rect_short$is_rectangular)
  expect_true(export_options_rect_long$is_rectangular)
  expect_false(export_options_regular_short$is_rectangular)
  expect_false(export_options_regular_long$is_rectangular)
})

# test meta names
test_that("Meta names available.", {
  expect_equal(as.vector(unlist(export_options_regular_short$meta_names)), c("fs", "cn", "ctr", "is",
                                                                             "qs", "qac",  "vp", "vpfs",
                                                                             "atcn", "atcvp", "cts", "miv", "cl"))
  expect_equal(as.vector(unlist(export_options_regular_long$meta_names)), c("forms", "casenodes",
                                                                            "centres", "items",
                                                                            "questions", "queries",
                                                                            "visitplan", "visitplanforms",
                                                                            "atcasenodes", "atcasevisitplans",
                                                                            "comments", "miv", "cl"))
})
