context("id conversion testing")

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

bmd <- load_export_table(data_dir = system.file("extdata",
                                                "s_export_CSV-xls_BMD.zip",
                                                package = "secuTrialR"),
                         file_name = "bmd.xls",
                         export_options = export_options,
                         add_pat_id = FALSE,
                         add_centre = FALSE)

# ---- test mnppid2mnpaid / using "sum" for simplicity here
test_that("mnpaids properly created.", {
  expect_equal(sum(mnppid2mnpaid(bmd$mnppid, patient_table = patient)), 135338)
})

# ---- test add_pat_id_col / using "sum" for simplicity here
bmd_with_patid <- add_pat_id_col(table = bmd, id = "pat.id", patient_table = patient)
test_that("pat.id column properly created.", {
  expect_equal(sum(bmd_with_patid$pat.id), 135338)
})

# ---- test mnppid2centre
test_that("centre properly mapped.", {
  expect_equal(as.numeric(table(mnppid2centre(bmd$mnppid, patient_table = patient, centre_table = centre))), 504)
})

# ---- test add_centre_col
test_that("centre column properly created.", {
  expect_equal(as.numeric(table(add_centre_col(bmd, patient_table = patient, centre_table = centre)$centre)), 504)
})

# ---- test remove_trailing_bracket
test_that("Only trailing brackets removed.", {
  expect_equal(remove_trailing_bracket("Hospital (Test Study)"), "Hospital")
  expect_equal(remove_trailing_bracket("(Test Study) Hospital"), "(Test Study) Hospital")
})
