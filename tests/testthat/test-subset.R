context("subset")

## compares two secutrialdata for equality. very specialized function for the purpose of testing.
## if equal, TRUE is returned, otherwise, FALSE.
## equality = "atr" ... equality of attributes in all elements of dat_ref and dat
## equality = "dat" ... equality of data contained in all elements of dat_ref and dat
## equality = "all" ... equality of data and attributes
secuTrial_is_equal <- function(dat_ref, dat, equality = "all"){
  if (!equality %in% c("all", "attr", "data")) stop("eqaulity must be all, attr or data")
  forms_equal <- TRUE
  attrs_equal <- TRUE
  for (tab in names(dat)){
    if (tab == "export_options") {
      next
    }
    attrs_equal <- suppressWarnings(forms_equal & all_equal(sort(names(attributes(dat[[tab]]))),
                                                            sort(names(attributes(dat_ref[[tab]])))))
    forms_equal <- suppressWarnings(attrs_equal & all_equal(dat[[tab]], dat_ref[[tab]]))
    if (equality == "all"){
      return(forms_equal & attrs_equal)
    } else if (equality == "attr"){
      return(attr_equal)
    } else {
      return(forms_equal)
    }
  }
}

## function that compares patient IDs present in all data frames
## of a "dat" secuTrialdata object with those provided via "patients" character vector
compare_patients <- function(dat, patients){
  pats_equal <- TRUE
  for (tab in names(dat)){
    if (tab == "export_options") {
      next
    }
    if (any(names(dat[[tab]]) %in% "pat_id") & nrow(dat[[tab]]) > 0){
      pats_equal <- pats_equal & all(dat[[tab]][["pat_id"]] %in% patients)
    } else if (any(names(dat[[tab]]) %in% "mnpaid") & nrow(dat[[tab]]) > 0){
      pats_equal <- pats_equal & all(dat[[tab]][["mnpaid"]] %in% patients)
    } else{
      next
    }
    if (!pats_equal){
      print(unique(dat[[tab]] %>% select(contains("pat_id"), contains("mnpaid"))))
      return(pats_equal)
    }
  }
  return(pats_equal)
}


sT <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                 "s_export_CSV-xls_CTU05_short_all-info_en.zip",
                                 package = "secuTrialR"))

sT_InselUSB <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                          "s_export_CSV-xls_CTU05_short_Insel-USB_en.zip",
                                          package = "secuTrialR"))

sT_noid <- read_secuTrial(system.file("extdata", "sT_exports", "export_options",
                                               "s_export_CSV-xls_CTU05_20191003-144833_no_addid.zip",
                                               package = "secuTrialR"))

sT_nocentre <- read_secuTrial(system.file("extdata", "sT_exports", "export_options",
                                      "s_export_CSV-xls_CTU05_20191003-144655_no_centre_info.zip",
                                      package = "secuTrialR"))

sT_long <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                      "s_export_CSV-xls_CTU05_long_all-info_en.zip",
                                      package = "secuTrialR"))

sT_InselUSB_long <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                               "s_export_CSV-xls_CTU05_long_Insel-USB_en.zip",
                                               package = "secuTrialR"))

test_that("Subset errors", {
  expect_error(subset_secuTrial(sT_noid, patient = "1")) # cannot subset based on patients if no "add id"
  expect_error(subset_secuTrial(sT_nocentre, patient = "1")) # cannot subset based on centres if no centre information
})

## test subsetting based on centers
patients <- c("RPACK-CBE-001", "RPACK-CBE-002", "RPACK-CBE-003", "RPACK-CBE-004",
              "RPACK-CBE-005", "RPACK-INS-014", "RPACK-INS-015")
centres <- c(461, 441)
sT_subset <- subset_secuTrial(sT, centre = centres) # positive selection of data based on centres
sT_subset_exclude <- subset_secuTrial(sT, centre = 462, exclude = TRUE) # negative selection of data based on centres
sT_subset_long <- subset_secuTrial(sT_long, centre = centres) # subsetting for exports with long table names

test_that("Subset centre", {
  expect_equal(sT, subset_secuTrial(sT)) # return input if no selection criteria provided (short table names)
  expect_equal(sT_long, subset_secuTrial(sT_long)) # return input if no selection criteria provided (long table names)
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB, sT_subset, "all")) # check that subsetting worked based on centres
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB, sT_subset_exclude, "all")) # check that centers are correctly excluded
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB_long, sT_subset_long, "all")) # as above for long table names
})

## test subsetting based on patients
patients <- c("RPACK-CBE-001", "RPACK-CBE-002", "RPACK-CBE-003", "RPACK-CBE-004",
              "RPACK-CBE-005", "RPACK-INS-014", "RPACK-INS-015")
sT_subset <- subset_secuTrial(sT, patient = patients) # positive selection of data based on patients
sT_subset_exclude <- subset_secuTrial(sT, patient = patients, exclude = TRUE) # negative selection of data based on patients

test_that("Subset patient", {
  expect_equal(TRUE, all(unique(sT_subset$baseline$pat_id) %in% patients)) # check if subsetting worked in "baseline" tbl
  expect_equal(TRUE, all(patients %in% unique(sT_subset$baseline$pat_id))) # check if subsetting worked in "baseline" tbl
  expect_equal(TRUE, compare_patients(sT_subset, patients)) # data frames of sT_subset should contain only "patients" data
  expect_equal(TRUE, compare_patients(sT_subset_exclude, sT$cn$mnpaid[!sT$cn$mnpaid %in% patients])) # exclude logics
  expect_equal(levels(sT$baseline$centre), levels(sT_subset$baseline$centre)) # center levels should stay same
  expect_equal(FALSE, all(sT$baseline$centre %in% sT_subset$baseline$centre)) # actually represented levels change
  expect_equal(TRUE, all(sT$ctr$mnpctrid %in% sT_subset$ctr$mnpctrid)) # all centres should be present
})

## test subsetting based on both patients and centres in one go
centres <- c("462", "441")
sT_subset <- subset_secuTrial(sT, patient = patients, centre = centres) # positive selection for patients and centres
sT_subset_exclude <- subset_secuTrial(sT, patient = patients, centre = centres, exclude = TRUE) # exlude patients and centres

test_that("Subset patient and centre", {
  expect_equal(TRUE, compare_patients(sT_subset, sT$cn[sT$cn[["mnpaid"]] %in%
                                                         patients & sT$cn[["mnpctrid"]] %in% centres, "mnpaid"]))
  expect_equal(TRUE, all(sT_subset$ctr$mnpctrid %in% centres))
  expect_equal(levels(sT_subset$baseline$centre),  sT$ctr[sT$ctr$mnpctrid %in% centres, "mnpctrname"])

  expect_equal(TRUE, compare_patients(sT_subset_exclude, sT$cn[!sT$cn[["mnpaid"]] %in%
                                                                 patients & !sT$cn[["mnpctrid"]] %in% centres, "mnpaid"]))
  expect_equal(TRUE, all(!sT_subset_exclude$ctr$mnpctrid %in% centres))
  expect_equal(levels(sT_subset_exclude$baseline$centre), sT$ctr[!sT$ctr$mnpctrid %in% centres, "mnpctrname"])
})

# check subsetting of disjunct
patients <- c("RPACK-USB-123") # this patient is in centre 441
centres <- c("461", "462") # "patients" are not associated with "centres"
sT_subset_disjunct <- subset_secuTrial(sT, patient = patients, centre = centres) # no patient form data should be present

test_that("Subset disjunct patient and centre", {
  expect_equal(TRUE, compare_patients(sT_subset_disjunct, c())) # metadata tables are present, but no patient data
})
