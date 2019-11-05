context("get")

sT <- read_secuTrial(system.file("extdata", "sT_exports", "export_options",
                                 "s_export_CSV-xls_CTU05_20191003-144349_all_info.zip",
                                 package = "secuTrialR"))

sT_noid <- read_secuTrial(system.file("extdata", "sT_exports", "export_options",
                                      "s_export_CSV-xls_CTU05_20191003-144833_no_addid.zip",
                                      package = "secuTrialR"))

sT_nocentre <- read_secuTrial(system.file("extdata", "sT_exports", "export_options",
                                          "s_export_CSV-xls_CTU05_20191003-144655_no_centre_info.zip",
                                          package = "secuTrialR"))

sT_noid_nocentre <- read_secuTrial(system.file("extdata", "sT_exports", "export_options",
                                          "s_export_CSV-xls_CTU05_20191004-101600_no_addid_no_centre_info_no_proj_setup.zip",
                                          package = "secuTrialR"))

test_that("Get patient error", {
  expect_error(get_participants(data.frame()))
})

test_that("Get patients", {
  expect_equal(c(11, 4), dim(get_participants(sT)))
  expect_equal(c(11, 3), dim(get_participants(sT_noid)))
  expect_equal(c(11, 2), dim(get_participants(sT_nocentre)))
  expect_equal(c(11, 1), dim(get_participants(sT_noid_nocentre)))
})
