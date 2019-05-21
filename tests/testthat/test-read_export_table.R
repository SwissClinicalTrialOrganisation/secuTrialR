context("load export table testing")

# load export options
export_options <- read_export_options(data_dir = system.file("extdata",
                                                             "s_export_CSV-xls_BMD.zip",
                                                             package = "secuTrialR"))

# load casenodes, centre, visitplan and bmd table
casenodes <- read_export_table(data_dir = system.file("extdata",
                                                    "s_export_CSV-xls_BMD.zip",
                                                    package = "secuTrialR"),
                             file_name = "cn.xls",
                             export_options = export_options,
                             is_meta_table = TRUE)

centre <- read_export_table(data_dir = system.file("extdata",
                                                   "s_export_CSV-xls_BMD.zip",
                                                   package = "secuTrialR"),
                            file_name = "ctr.xls",
                            export_options = export_options,
                            is_meta_table = TRUE)

visitplan <- read_export_table(data_dir = system.file("extdata",
                                                      "s_export_CSV-xls_BMD.zip",
                                                      package = "secuTrialR"),
                               file_name = "vp.xls",
                               export_options = export_options,
                               is_meta_table = TRUE)

bmd_all <- read_export_table(data_dir = system.file("extdata",
                                                    "s_export_CSV-xls_BMD.zip",
                                                     package = "secuTrialR"),
                             file_name = "bmd.xls",
                             export_options = export_options,
                             casenodes_table = casenodes,
                             centre_table = centre,
                             visitplan_table = visitplan)

bmd_no_patid_ctr_vp <- read_export_table(data_dir = system.file("extdata",
                                                                "s_export_CSV-xls_BMD.zip",
                                                                package = "secuTrialR"),
                                         file_name = "bmd.xls",
                                         export_options = export_options,
                                         add_pat_id = FALSE,
                                         add_centre = FALSE,
                                         add_visitname = FALSE,
                                         casenodes_table = casenodes,
                                         centre_table = centre,
                                         visitplan_table = visitplan)

# test dimensions
test_that("All dimensions as expected.", {
  expect_equal(dim(casenodes), c(113, 13))
  expect_equal(dim(centre), c(1, 3))
  expect_equal(dim(visitplan), c(1, 10))
  expect_equal(dim(bmd_all), c(504, 27))
  expect_equal(dim(bmd_no_patid_ctr_vp), c(504, 24))
})

export_options_wrong_zip <- export_options
export_options_wrong_zip$is_zip <- "thatsnotit"

# test exceptions
test_that("Exceptions trigger as expected.", {
  expect_error(read_export_table(data_dir = system.file("extdata",
                                                        "s_export_CSV-xls_BMD.zip",
                                                        package = "secuTrialR"),
                                 file_name = "bmd.xls",
                                 export_options = export_options_wrong_zip,
                                 casenodes_table = casenodes,
                                 centre_table = centre,
                                 visitplan_table = visitplan))
})
