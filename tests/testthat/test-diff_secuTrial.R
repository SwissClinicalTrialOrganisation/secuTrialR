context("diff secuTrial exports")

skip_on_cran()

ctu05_en_1 <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                         "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                         package = "secuTrialR"))
#> ctu05_en_1$export_options$project_version
#[1] "(30.04.2019 - 13:40:52 (CEST))"

ctu05_en_2 <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                         "s_export_CSV-xls_CTU05_short_ref_miss_en_utf8.zip",
                                         package = "secuTrialR"))
#> ctu05_en_2$export_options$project_version
#[1] "(20.06.2019 - 11:22:04 (CEST))"

ctu05_fr <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                       "s_export_CSV-xls_CTU05_short_ref_miss_fr_iso8859-1.zip",
                                       package = "secuTrialR"))
#> ctu05_fr$export_options$project_version
#[1] "(20.06.2019 - 11:22:04 (CEST))"

bmd <- read_secuTrial(system.file("extdata", "sT_exports", "BMD",
                                  "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                  package = "secuTrialR"))

# v1 is essentially a clone of the CTU05 setup
ctu06_v1 <- read_secuTrial(system.file("extdata", "sT_exports", "change_tracking",
                                       "s_export_CSV-xls_CTU06_version1.zip",
                                       package = "secuTrialR"))
# v2 contains 2 additional forms (mnpctu06anewform, mnpctu06anothernewform)
# and 2 additional variables (new_item_in_fu, new_item_in_new_form)
ctu06_v2 <- read_secuTrial(system.file("extdata", "sT_exports", "change_tracking",
                                       "s_export_CSV-xls_CTU06_version2.zip",
                                       package = "secuTrialR"))
# v3 contains the changes in v2 and add one more variable (yet_another_item)
ctu06_v3 <- read_secuTrial(system.file("extdata", "sT_exports", "change_tracking",
                                       "s_export_CSV-xls_CTU06_version3.zip",
                                       package = "secuTrialR"))

test_that("Errors", {
  expect_error(diff_secuTrial("asd", bmd))
  expect_error(diff_secuTrial(ctu05_en_1, bmd))
  expect_error(diff_secuTrial(bmd, ctu05_fr))
})

print_string <- "The project structure has not changed. Project versions are both: (20.06.2019 - 11:22:04 (CEST))"
test_that("Same version", {
  expect_equal(diff_secuTrial(ctu05_en_2, ctu05_fr), print_string)
  expect_equal(diff_secuTrial(ctu05_fr, ctu05_fr), print_string)
})

# different version but no changes
test_that("Different version", {
  expect_equal(length(diff_secuTrial(ctu05_en_1, ctu05_fr)[[1]]), 0)
  expect_equal(length(diff_secuTrial(ctu05_en_1, ctu05_fr)[[2]]), 0)
})


# different versions with changes
v1_vs_v2 <- diff_secuTrial(ctu06_v1, ctu06_v2)
v1_vs_v3 <- diff_secuTrial(ctu06_v1, ctu06_v3)
v2_vs_v3 <- diff_secuTrial(ctu06_v2, ctu06_v3)

test_that("Different version and changes", {
  expect_equal(v1_vs_v2$new_forms, c("mnpctu06anewform", "mnpctu06anothernewform"))
  expect_equal(v1_vs_v2$new_variables, c("new_item_in_fu", "new_item_in_new_form"))
  expect_equal(v1_vs_v3$new_forms, c("mnpctu06anewform", "mnpctu06anothernewform"))
  expect_equal(v1_vs_v3$new_variables, c("yet_another_item", "new_item_in_fu", "new_item_in_new_form"))
  expect_equal(length(v2_vs_v3$new_forms), 0)
  expect_equal(v2_vs_v3$new_variables, "yet_another_item")
})
