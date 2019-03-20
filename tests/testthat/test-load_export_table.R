context("load export table testing")

# load export options
export_options <- load_export_options(data_dir = system.file("extdata",
                                                             "s_export_CSV-xls_BMD.zip",
                                                             package = "secuTrialR"))

# load patient, centre and bmd table
patient <- load_export_table(data_dir = system.file("extdata",
                                                    "s_export_CSV-xls_BMD.zip",
                                                    package = "secuTrialR"),
                             file_name = "cn.xls",
                             export_options = export_options)

centre <- load_export_table(data_dir = system.file("extdata",
                                                   "s_export_CSV-xls_BMD.zip",
                                                   package = "secuTrialR"),
                            file_name = "ctr.xls",
                            export_options = export_options)

bmd_all <- load_export_table(data_dir = system.file("extdata",
                                                    "s_export_CSV-xls_BMD.zip",
                                                     package = "secuTrialR"),
                             file_name = "bmd.xls",
                             export_options = export_options,
                             patient_table = patient,
                             centre_table = centre)

bmd_no_patid_ctr <- load_export_table(data_dir = system.file("extdata",
                                                             "s_export_CSV-xls_BMD.zip",
                                                             package = "secuTrialR"),
                                      file_name = "bmd.xls",
                                      export_options = export_options,
                                      add_pat_id = FALSE,
                                      add_centre = FALSE,
                                      patient_table = patient,
                                      centre_table = centre)


# test dimensions
test_that("All dimensions as expected.", {
  expect_equal(dim(patient), c(113, 14))
  expect_equal(dim(centre), c(1, 3))
  expect_equal(dim(bmd_all), c(504, 26))
  expect_equal(dim(bmd_no_patid_ctr), c(504, 24))
})
