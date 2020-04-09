context("get")

skip_on_cran()

sT <- read_secuTrial(system.file("extdata", "sT_exports", "exp_opt",
                                 "s_export_CSV-xls_CTU05_all_info.zip",
                                 package = "secuTrialR"))

sT_noid <- read_secuTrial(system.file("extdata", "sT_exports", "exp_opt",
                                      "s_export_CSV-xls_CTU05_no_addid.zip",
                                      package = "secuTrialR"))

sT_nocentre <- read_secuTrial(system.file("extdata", "sT_exports", "exp_opt",
                                          "s_export_CSV-xls_CTU05_no_centre_info.zip",
                                          package = "secuTrialR"))

sT_noid_nocentre <- read_secuTrial(system.file("extdata", "sT_exports", "exp_opt",
                                          "s_export_CSV-xls_CTU05_no_addid_ctr_info_proj_setup.zip",
                                          package = "secuTrialR"))

test_that("Get participant error", {
  expect_error(get_participants(data.frame()))
})

test_that("Get participants", {
  expect_equal(c(11, 4), dim(get_participants(sT)))
  expect_equal(c(11, 3), dim(get_participants(sT_noid)))
  expect_equal(c(11, 2), dim(get_participants(sT_nocentre)))
  expect_equal(c(11, 1), dim(get_participants(sT_noid_nocentre)))
})
