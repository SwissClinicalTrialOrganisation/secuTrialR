context("subset")

sT <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                 "s_export_CSV-xls_CTU05_short_all-info_en.zip",
                                 package = "secuTrialR"))

sT_InselUSB <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                          "s_export_CSV-xls_CTU05_short_Insel-USB_en.zip",
                                          package = "secuTrialR"))
sT_InselUSB_noid <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                                "s_export_CSV-xls_CTU05_short_Insel-USB_no-id_en.zip",
                                                package = "secuTrialR"))

forms_equal <- TRUE
attr_equal <- TRUE
subset_sT <- subset_secuTrial(sT, centre = c(461, 441))

for(tab in names(sT_InselUSB)){
  if (tab == "export_options") {
    next
  }
  forms_equal <- suppressWarnings(forms_equal & all_equal(sort(names(attributes(subset_sT[[tab]]))), sort(names(attributes(sT_InselUSB[[tab]])))))
  attr_equal <- suppressWarnings(attr_equal & all_equal(subset_sT[[tab]], sT_InselUSB[[tab]]))
}

test_that("Subset", {
  expect_equal(sT, subset_secuTrial(sT))
  expect_equal(TRUE, forms_equal)
  expect_equal(TRUE, attr_equal)
})
