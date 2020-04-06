context("read export table testing")

skip_on_cran()

# load export options
export_options <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                             "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                             package = "secuTrialR"))

# load export options unzipped
export_options_unzipped <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                                      "s_export_CSV-xls_BMD_short_en_utf8",
                                                                      package = "secuTrialR"))

# load casenodes, centre, visitplan and bmd table
casenodes <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                    "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                    package = "secuTrialR"),
                             file_name = "cn.xls",
                             export_options = export_options,
                             is_meta_table = TRUE)
# unzipped
casenodes_unzipped <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                               "s_export_CSV-xls_BMD_short_en_utf8",
                                                               package = "secuTrialR"),
                                        file_name = "cn.xls",
                                        export_options = export_options_unzipped,
                                        is_meta_table = TRUE)

centre <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                   "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                   package = "secuTrialR"),
                            file_name = "ctr.xls",
                            export_options = export_options,
                            is_meta_table = TRUE)

# unzipped
centre_unzipped <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                            "s_export_CSV-xls_BMD_short_en_utf8",
                                                            package = "secuTrialR"),
                                     file_name = "ctr.xls",
                                     export_options = export_options_unzipped,
                                     is_meta_table = TRUE)


visitplan <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                      "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                      package = "secuTrialR"),
                               file_name = "vp.xls",
                               export_options = export_options,
                               is_meta_table = TRUE)

# unzipped
visitplan_unzipped <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                               "s_export_CSV-xls_BMD_short_en_utf8",
                                                               package = "secuTrialR"),
                                        file_name = "vp.xls",
                                        export_options = export_options_unzipped,
                                        is_meta_table = TRUE)

# specifically setting this here
# because the availability of the add_id is
# figured out in read_secuTrail_raw()
export_options$add_id <- TRUE
bmd_all <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                    "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                     package = "secuTrialR"),
                             file_name = "bmd.xls",
                             export_options = export_options,
                             casenodes_table = casenodes,
                             centre_table = centre,
                             visitplan_table = visitplan)

# specifically setting this here
# because the availability of the add_id is
# figured out in read_secuTrail_raw()
export_options_unzipped$add_id <- TRUE
# unzipped
bmd_all_unzipped <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                             "s_export_CSV-xls_BMD_short_en_utf8",
                                                             package = "secuTrialR"),
                                      file_name = "bmd.xls",
                                      export_options = export_options_unzipped,
                                      casenodes_table = casenodes_unzipped,
                                      centre_table = centre_unzipped,
                                      visitplan_table = visitplan_unzipped)

bmd_no_patid_ctr_vp <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                                "s_export_CSV-xls_BMD_short_en_utf8.zip",
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

# test zipped and unzipped for equality
test_that("Zipped and unzipped return the same.", {
  expect_true(all.equal(casenodes, casenodes_unzipped))
  expect_true(all.equal(centre, centre_unzipped))
  expect_true(all.equal(visitplan, visitplan_unzipped))
  expect_true(all.equal(bmd_all, bmd_all_unzipped))
})

# test exceptions
test_that("Exceptions trigger as expected.", {
  expect_error(read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                        "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                        package = "secuTrialR"),
                                 file_name = "bmd.xls",
                                 export_options = export_options_wrong_zip,
                                 casenodes_table = casenodes,
                                 centre_table = centre,
                                 visitplan_table = visitplan))
})
