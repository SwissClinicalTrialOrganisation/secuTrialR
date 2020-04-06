context("annual recruitment")

skip_on_cran()

sdat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "snames",
                                         "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                         package = "secuTrialR"))
ldat_ctu05 <- read_secuTrial(system.file("extdata", "sT_exports", "lnames",
                                         "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                         package = "secuTrialR"))
bmd <- read_secuTrial(system.file("extdata", "sT_exports", "BMD",
                                  "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                  package = "secuTrialR"))
# warning can be suppressed (it is expected)
suppressWarnings(
tes05 <- read_secuTrial(system.file("extdata", "sT_exports", "encodings",
                                    "s_export_CSV-xls_TES05_long_ref_en_utf8.zip",
                                    package = "secuTrialR"))
)

test_that("Test fail", {
  expect_error(annual_recruitment(1337))
})

test_that("Test output", {
  expect_equal(annual_recruitment(sdat_ctu05),
               annual_recruitment(ldat_ctu05))
  expect_equal(annual_recruitment(sdat_ctu05),
               annual_recruitment(ldat_ctu05))
  expect_equal(sum(as.numeric(annual_recruitment(sdat_ctu05)$Total[2:4])), 11)
  expect_equal(sum(as.numeric(annual_recruitment(sdat_ctu05)$"2018"[2:4])), 1)
  expect_equal(sum(as.numeric(annual_recruitment(sdat_ctu05)$"2019"[2:4])), 10)
  expect_equal(annual_recruitment(tes05)$Total, "7")
  expect_equal(annual_recruitment(bmd)$Total, "113")
  expect_equal(dim(annual_recruitment(ldat_ctu05)), c(4, 4))
  expect_equal(dim(annual_recruitment(bmd)), c(1, 4))
  expect_equal(dim(annual_recruitment(tes05)), c(1, 4))
})

# subset_secuTrial tests for plot_recruitment
# centres
sdat_ctu05_bern <- subset_secuTrial(sdat_ctu05, centre = "Inselspital Bern (RPACK)")
sdat_ctu05_no_bern <- subset_secuTrial(sdat_ctu05, centre = "Inselspital Bern (RPACK)", exclude = TRUE)
ldat_ctu05_bern <- subset_secuTrial(ldat_ctu05, centre = "Inselspital Bern (RPACK)")
ldat_ctu05_no_bern <- subset_secuTrial(ldat_ctu05, centre = c("Charité Berlin (RPACK)",
                                                              "Universitätsspital Basel (RPACK)"))
ann_rec_all <- annual_recruitment(sdat_ctu05)
ann_rec_l_bern <- annual_recruitment(ldat_ctu05_bern)
ann_rec_s_bern <- annual_recruitment(sdat_ctu05_bern)
ann_rec_l_no_bern <- annual_recruitment(ldat_ctu05_no_bern)
ann_rec_s_no_bern <- annual_recruitment(sdat_ctu05_no_bern)

test_that("Test centre subsetting", {
  expect_equal(ann_rec_l_bern, ann_rec_s_bern)
  expect_equal(ann_rec_l_no_bern, ann_rec_s_no_bern)
  expect_true("Inselspital Bern (RPACK)" %in% ann_rec_l_bern$Center)
  expect_false("Inselspital Bern (RPACK)" %in% ann_rec_l_no_bern$Center)
  expect_equal((as.numeric(ann_rec_all$Total)[1] - as.numeric(ann_rec_s_bern$Total)[1]),
               as.numeric(ann_rec_s_no_bern$Total)[1])
})

# participants
id_set <- c("RPACK-CBE-001", "RPACK-INS-012", "RPACK-USB-123")

rm_set_sdat <- subset_secuTrial(sdat_ctu05, participant = id_set, exclude = TRUE)
keep_set_sdat <- subset_secuTrial(sdat_ctu05, participant = id_set)

ann_rec_rm <- annual_recruitment(rm_set_sdat)
annrec_keep <- annual_recruitment(keep_set_sdat)

test_that("Test participant subsetting", {
  expect_equal((as.numeric(ann_rec_all$Total) - as.numeric(annrec_keep$Total)),
               as.numeric(ann_rec_rm$Total))
})
