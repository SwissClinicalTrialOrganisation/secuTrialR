context("diff secuTrial exports")

ctu05_en_1 <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
                                         "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                         package = "secuTrialR"))
#> ctu05_en_1$export_options$project_version
#[1] "(30.04.2019 - 13:40:52 (CEST))"

ctu05_en_2 <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
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
