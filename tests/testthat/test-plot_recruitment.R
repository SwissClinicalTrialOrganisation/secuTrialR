context("plot recruitment")

sdat <- read_secuTrial(system.file("extdata", "sT_exports", "shortnames",
                                   "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata", "sT_exports", "longnames",
                                  "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                  package = "secuTrialR"))

ctu05_utf8_french <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                                "s_export_CSV-xls_CTU05_short_ref_miss_fr_utf8.zip",
                                                package = "secuTrialR"))
ctu05_iso15_french <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                                 "s_export_CSV-xls_CTU05_short_ref_miss_fr_iso8859-15.zip",
                                                 package = "secuTrialR"))

test_that("Test fail", {
  expect_error(plot_recruitment(1337))
})

test_that("Test output", {
  # show_centres = FALSE
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = FALSE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = FALSE))
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = FALSE),
               plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = FALSE))
  expect_equal(plot_recruitment(ctu05_utf8_french, return_data = TRUE, show_centres = FALSE),
               plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = FALSE))
  # show_centres = TRUE
  expect_equal(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = TRUE))
  expect_equal(plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = TRUE),
               plot_recruitment(ldat, return_data = TRUE, show_centres = TRUE))
  expect_equal(plot_recruitment(ctu05_iso15_french, return_data = TRUE, show_centres = TRUE),
               plot_recruitment(ctu05_utf8_french, return_data = TRUE, show_centres = TRUE))
  expect_equal(dim(plot_recruitment(ldat, return_data = TRUE, show_centres = FALSE)[[1]]), c(11, 4))
  # [[1]] has all data, row count in all other entries summed up should be equal to it
  expect_equal(nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[1]]),
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[2]]) +
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[3]]) +
               nrow(plot_recruitment(sdat, return_data = TRUE, show_centres = TRUE)[[4]])
               )
})

# subset_secuTrial tests for plot_recruitment
sdat_bern <- subset_secuTrial(sdat, centre = 461)
sdat_berlin <- subset_secuTrial(sdat, centre = 462)
sdat_no_basel_1 <- subset_secuTrial(sdat, centre = c(461, 462))
sdat_no_basel_2 <- subset_secuTrial(sdat, centre = 441, exclude = TRUE)

rec_sdat_all <- plot_recruitment(sdat, return_data = TRUE)
rec_sdat_bern <- plot_recruitment(sdat_bern, return_data = TRUE)
rec_sdat_berlin <- plot_recruitment(sdat_berlin, return_data = TRUE)
rec_sdat_no_basel_1 <- plot_recruitment(sdat_no_basel_1, return_data = TRUE)
rec_sdat_no_basel_2 <- plot_recruitment(sdat_no_basel_2, return_data = TRUE)

test_that("Test output after subsetting centres", {
  expect_equal(length(rec_sdat_all), length(rec_sdat_bern) + 2)
  expect_equal(length(rec_sdat_all), length(rec_sdat_berlin) + 2)
  expect_equal(length(rec_sdat_no_basel_1), length(rec_sdat_berlin) + 1)
  expect_equal(length(unique(rec_sdat_bern[[1]]$centre_name)), 1)
  expect_equal(length(unique(rec_sdat_berlin[[1]]$centre_name)), 1)
  expect_equal(rec_sdat_no_basel_1, rec_sdat_no_basel_2)
  expect_false("Universitätsspital Basel (RPACK)" %in% rec_sdat_no_basel_1[[1]]$centre_name)
  expect_true("Inselspital Bern (RPACK)" %in% rec_sdat_no_basel_1[[1]]$centre_name)
  expect_true("Charité Berlin (RPACK)" %in% rec_sdat_no_basel_1[[1]]$centre_name)
  expect_equal(rec_sdat_all[[2]], rec_sdat_berlin[[1]])
  expect_equal(rec_sdat_all[[3]]$centre_name, rec_sdat_bern[[1]]$centre_name)
})

# 11 registered cases in sdat
id_set <- c("RPACK-CBE-002", "RPACK-INS-014", "RPACK-INS-011")

rm_set_sdat <- subset_secuTrial(sdat, patient = id_set, exclude = TRUE)
keep_set_sdat <- subset_secuTrial(sdat, patient = id_set)

rm_set_rec <- plot_recruitment(rm_set_sdat, return_data = TRUE)
keep_set_rec <- plot_recruitment(keep_set_sdat, return_data = TRUE)

test_that("Test output after subsetting patients", {
  expect_equal(length(rm_set_rec), length(rec_sdat_all))
  expect_equal(length(keep_set_rec), length(rec_sdat_all) - 1)
  expect_equal(nrow(rec_sdat_all[[1]]), nrow(rm_set_rec[[1]]) + 3)
  expect_equal(nrow(rec_sdat_all[[1]]), nrow(keep_set_rec[[1]]) + 8)
  expect_true("Universitätsspital Basel (RPACK)" %in% rm_set_rec[[1]]$centre_name)
  expect_false("Universitätsspital Basel (RPACK)" %in% keep_set_rec[[1]]$centre_name)
})
