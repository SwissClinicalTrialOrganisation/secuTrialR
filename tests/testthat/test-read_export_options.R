context("load export options testing")

skip_on_cran()

# shorten table names
export_options_regular_short <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                                           "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                                           package = "secuTrialR"))
# long table names
export_options_regular_long <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                                          "s_export_CSV-xls_BMD_long_en_utf8.zip",
                                                                          package = "secuTrialR"))
# rectangular shorten table names
export_options_rect_short <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                                        "s_export_CSV-xls_BMD_rt_short_en_utf8.zip",
                                                                        package = "secuTrialR"))
# rectangular long table names
export_options_rect_long <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                                       "s_export_CSV-xls_BMD_rt_long_en_utf8.zip",
                                                                       package = "secuTrialR"))
# unzipped
bmd_unzipped <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                           "s_export_CSV-xls_BMD_short_en_utf8",
                                                           package = "secuTrialR"))

# duplicated meta
dup_meta <- read_export_options(system.file("extdata", "sT_exports", "lnames",
                                            "s_export_CSV-xls_CTU05_long_meta_ref_miss_en_utf8.zip",
                                            package = "secuTrialR"))

# ISO-8859-15
exp_opt_tes05_iso <- read_export_options(system.file("extdata", "sT_exports", "encodings",
                                                     "s_export_CSV-xls_TES05_short_ref_en_iso8859-15.zip",
                                                     package = "secuTrialR"))

# test encoding
test_that("Encoding parsed as expected.", {
  expect_equal(export_options_regular_short$encoding, "UTF-8")
  expect_equal(bmd_unzipped$encoding, "UTF-8")
  expect_equal(exp_opt_tes05_iso$encoding, "ISO-8859-15")
})

# test shortened table names
test_that("Shorten names identified.", {
  expect_true(bmd_unzipped$short_names)
  expect_true(export_options_regular_short$short_names)
  expect_false(export_options_regular_long$short_names)
  expect_true(export_options_rect_short$short_names)
  expect_false(export_options_rect_long$short_names)
})

# test zip
test_that("zip archive ending identified.", {
  expect_false(bmd_unzipped$is_zip)
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
  expect_false(bmd_unzipped$is_rectangular)
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

# prepare path to example export
export_location <- system.file("extdata", "sT_exports", "BMD",
                               "s_export_CSV-xls_BMD_short_en_utf8.zip",
                               package = "secuTrialR")

# load all export data
sT_export <- read_secuTrial_raw(data_dir = export_location)

# capture the print
captured_print <- capture.output(print(sT_export$export_options))

# test print.secutrialoptions
test_that("Print export options working.", {
  expect_equal(captured_print[1], "secuTrial version: 5.3.4.6 ")
  expect_equal(captured_print[2], "Time of export on server: 25.02.2019 - 15:14:27 (CET) ")
  expect_equal(captured_print[6], "Seperator: '\t'")
  expect_equal(captured_print[7], "14 files exported")
  expect_equal(captured_print[9], "Reference values not exported - factorize not possible")
})


sT_export2 <- read_secuTrial_raw(data_dir = system.file("extdata", "sT_exports", "snames",
                                                           "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                                           package = "secuTrialR"))
# project version
test_that("Project version parsing", {
  expect_equal(sT_export$export_options$project_version, "(25.02.2019 - 13:13:44 (CET))")
  expect_equal(export_options_regular_short$project_version, "(25.02.2019 - 13:13:44 (CET))")
  expect_equal(export_options_regular_long$project_version, "(25.02.2019 - 13:13:44 (CET))")
  expect_equal(sT_export2$export_options$project_version, "(30.04.2019 - 13:40:52 (CEST))")
  expect_equal(bmd_unzipped$project_version, "(25.02.2019 - 13:13:44 (CET))")
})

# project name
test_that("Project name parsing", {
  expect_equal(sT_export$export_options$project_name, "BONE MINERAL DENSITY")
  expect_equal(export_options_regular_short$project_name, "BONE MINERAL DENSITY")
  expect_equal(export_options_regular_long$project_name, "BONE MINERAL DENSITY")
  expect_equal(sT_export2$export_options$project_name, "secuTrialR example CDMA")
  expect_equal(bmd_unzipped$project_name, "BONE MINERAL DENSITY")
})

# duplicated meta data
test_that("Project version parsing", {
  expect_false(sT_export$export_options$duplicate_meta)
  expect_false(export_options_regular_short$duplicate_meta)
  expect_false(export_options_regular_long$duplicate_meta)
  expect_false(sT_export2$export_options$duplicate_meta)
  expect_false(bmd_unzipped$duplicate_meta)
  expect_true(dup_meta$duplicate_meta)
})

# test time of export
# manually checked all of these in the respective ExportOptions.html files
test_that("Time of export", {
  expect_equal(sT_export$export_options$time_of_export, "25.02.2019 - 15:14:27 (CET)")
  expect_equal(export_options_regular_long$time_of_export, "18.03.2019 - 10:47:03 (CET)")
  expect_equal(sT_export2$export_options$time_of_export, "30.04.2019 - 15:29:45 (CEST)")
})

# errors for non CSV exports
test_that("Errored for non CSV format", {
  # SAS
  expect_error(read_export_options(data_dir = system.file("extdata", "sT_exports", "exp_opt",
                                                          "s_export_SAS_CTU05_20191115-092453_SAS.zip",
                                                          package = "secuTrialR")))
  # SPSS
  expect_error(read_export_options(data_dir = system.file("extdata", "sT_exports", "exp_opt",
                                                          "s_export_SPSS_CTU05_20191115-092020_SPSS.zip",
                                                          package = "secuTrialR")))
  # CDISC
  expect_error(read_export_options(data_dir = system.file("extdata", "sT_exports", "exp_opt",
                                                          "s_export_XML_CTU05_20191115-092559_CDISC.zip",
                                                          package = "secuTrialR")))
})

# success for CSV exports
eo_csv <- read_export_options(data_dir = system.file("extdata", "sT_exports", "exp_opt",
                                                     "s_export_CSV_CTU05_20191115-091627_CSV.zip",
                                                     package = "secuTrialR"))
test_that("Success for CSV format", {
  expect_equal(eo_csv$format_info, "CSV format")
  expect_equal(exp_opt_tes05_iso$format_info, "CSV format for MS Excel")
})

# audit trail parsing
export_location_only_col_names <- system.file("extdata", "sT_exports", "exp_opt",
                                              "s_export_CSV-xls_CTU05_only_column_names.zip",
                                              package = "secuTrialR")

sT_export_only_col_names <- read_secuTrial_raw(data_dir = export_location_only_col_names)

test_that("Lack of Audit Trail successfully parsed", {
  expect_false(sT_export_only_col_names$export_options$audit_trail)
})

# hidden field parsing
test_that("Hidden fields successfully parsed", {
  expect_false(sT_export_only_col_names$export_options$hidden_fields)
  expect_true(exp_opt_tes05_iso$hidden_fields)
})

# Form meta data Structure option parsing
test_that("Structure fields successfully parsed", {
  expect_false(sT_export_only_col_names$export_options$structure)
  expect_true(exp_opt_tes05_iso$structure)
})
