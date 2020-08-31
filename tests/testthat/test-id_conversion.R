context("id conversion testing")

skip_on_cran()

# load export options
export_options <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                             "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                             package = "secuTrialR"))

# load casenodes, centre and bmd table
casenodes <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                    "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                    package = "secuTrialR"),
                             file_name = "cn.xls",
                             export_options = export_options,
                             is_meta_table = TRUE)

centre <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                   "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                   package = "secuTrialR"),
                            file_name = "ctr.xls",
                            export_options = export_options,
                            is_meta_table = TRUE)

visitplan <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                      "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                      package = "secuTrialR"),
                               file_name = "vp.xls",
                               export_options = export_options,
                               is_meta_table = TRUE)

bmd <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                package = "secuTrialR"),
                         file_name = "bmd.xls",
                         export_options = export_options,
                         add_pat_id = FALSE,
                         add_centre = FALSE,
                         add_visitname = FALSE)

# ---- test errors
test_that("failure", {
  expect_error(add_pat_id_col(iris))
  expect_error(add_visitname_col(iris))
})

# ---- test mnppid2mnpaid / using "sum" for simplicity here
test_that("mnpaids properly created.", {
  expect_equal(sum(mnppid2mnpaid(bmd$mnppid, casenodes_table = casenodes)), 135338)
})

# ---- test add_pat_id_col / using "sum" for simplicity here
bmd_with_patid <- add_pat_id_col(table = bmd, id = "pat_id", casenodes_table = casenodes)
test_that("pat_id column properly created.", {
  expect_equal(sum(bmd_with_patid$pat_id), 135338)
})

# ---- test mnppid2centre
test_that("centre properly mapped.", {
  expect_equal(as.numeric(table(mnppid2centre(bmd$mnppid, casenodes_table = casenodes, centre_table = centre))), 504)
})

# ---- test add_centre_col
test_that("centre column properly created.", {
  expect_equal(as.numeric(table(add_centre_col(bmd, casenodes_table = casenodes, centre_table = centre)$centre)), 504)
})

# ---- test mnpvisid2mnpvislabel
test_that("visitlabel properly mapped.", {
  expect_equal(as.numeric(table(mnpvisid2mnpvislabel(bmd$mnpvisid, visitplan_table = visitplan))), 504)
})

# ---- test add_visitname_col
test_that("visit_name column properly created.", {
  expect_equal(as.numeric(table(add_visitname_col(bmd, visitplan_table = visitplan)$visit_name)), 504)
})

# ---- test remove_trailing_bracket
test_that("Only trailing brackets removed.", {
  expect_equal(remove_trailing_bracket("Hospital (Test Study)"), "Hospital")
  expect_equal(remove_trailing_bracket("(Test Study) Hospital"), "(Test Study) Hospital")
})
