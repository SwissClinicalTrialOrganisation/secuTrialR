context("subset")

secuTrial_is_equal <- function(dat_ref, dat, equality = "all"){
  if (!equality %in% c("all", "attr", "data")) stop("eqaulity must be all, attr or data")
  forms_equal <- TRUE
  attrs_equal <- TRUE
  for(tab in names(dat)){
    if (tab == "export_options") {
      next
    }
    attrs_equal <- suppressWarnings(forms_equal & all_equal(sort(names(attributes(dat[[tab]]))), sort(names(attributes(dat_ref[[tab]])))))
    forms_equal <- suppressWarnings(attrs_equal & all_equal(dat[[tab]], dat_ref[[tab]]))
    if(equality == "all"){
      return(forms_equal & attrs_equal)
    } else if (equality == "attr"){
      return(attr_equal)
    } else {
      return(forms_equal)
    }
  }
}

compare_patients <- function(dat, patients){
  pats_equal <- TRUE
  for(tab in names(dat)){
    if (tab == "export_options") {
      next
    }
    if (any(names(dat[[tab]]) %in% "pat_id") & nrow(dat[[tab]]) > 0){
      pats_equal <- pats_equal & all(dat[[tab]][["pat_id"]] %in% patients)
    } else if(any(names(dat[[tab]]) %in% "mnpaid") & nrow(dat[[tab]]) > 0){
      pats_equal <- pats_equal & all(dat[[tab]][["mnpaid"]] %in% patients)
    } else{
      next
    }
    if(!pats_equal){
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
sT_InselUSB_noid <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                               "s_export_CSV-xls_CTU05_short_Insel-USB_no-id_en.zip",
                                               package = "secuTrialR"))

test_that("Subset errors", {
  expect_error(subset_secuTrial(sT_InselUSB_noid, patient = "1"))
})

## subsetting based on centers
patients <- c("RPACK-CBE-001", "RPACK-CBE-002", "RPACK-CBE-003", "RPACK-CBE-004",
              "RPACK-CBE-005", "RPACK-INS-014", "RPACK-INS-015")
centres <- c(461, 441)
sT_subset <- subset_secuTrial(sT, centre = centres)
sT_subset_exclude <- subset_secuTrial(sT, centre = 462, exclude = TRUE)

test_that("Subset centre", {
  expect_equal(sT, subset_secuTrial(sT))
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB, sT_subset, "all"))
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB, sT_subset_exclude, "all"))
})

## based on patients
patients <- c("RPACK-CBE-001", "RPACK-CBE-002", "RPACK-CBE-003", "RPACK-CBE-004",
              "RPACK-CBE-005", "RPACK-INS-014", "RPACK-INS-015")
sT_subset <- subset_secuTrial(sT, patient = patients)
sT_subset_exclude <- subset_secuTrial(sT, patient = patients, exclude = TRUE)

test_that("Subset patient", {
  expect_equal(TRUE, all(unique(sT_subset$baseline$pat_id) %in% patients))
  expect_equal(TRUE, all(patients %in% unique(sT_subset$baseline$pat_id)))
  expect_equal(TRUE, compare_patients(sT_subset, patients))
  expect_equal(TRUE, compare_patients(sT_subset_exclude, sT$cn$mnpaid[!sT$cn$mnpaid %in% patients]))
  expect_equal(levels(sT$baseline$centre), levels(sT_subset$baseline$centre)) ## center levels should stay same
  expect_equal(FALSE, all(sT$baseline$centre %in% sT_subset$baseline$centre)) ## actually represented levels not
  expect_equal(TRUE, all(sT$ctr$mnpctrid %in% sT_subset$ctr$mnpctrid)) ## all centres should be present, although no patients given for some centres
})

centres <- c("462", "441")
sT_subset <- subset_secuTrial(sT, patient = patients, centre = centres)
sT_subset_exclude <- subset_secuTrial(sT, patient = patients, centre = centres, exclude = TRUE)
## what happens if disjunkt

test_that("Subset patient and centre", {
  expect_equal(TRUE, compare_patients(sT_subset, sT$cn[sT$cn[["mnpaid"]] %in% patients & sT$cn[["mnpctrid"]] %in% centres, "mnpaid"]))
  expect_equal(TRUE, all(sT_subset$ctr$mnpctrid %in% centres))
  expect_equal(levels(sT_subset$baseline$centre),  sT$ctr[sT$ctr$mnpctrid %in% centres, "mnpctrname"])

  expect_equal(TRUE, compare_patients(sT_subset_exclude, sT$cn[!sT$cn[["mnpaid"]] %in% patients & !sT$cn[["mnpctrid"]] %in% centres, "mnpaid"]))
  expect_equal(TRUE, all(!sT_subset_exclude$ctr$mnpctrid %in% centres))
  expect_equal(levels(sT_subset_exclude$baseline$centre), sT$ctr[!sT$ctr$mnpctrid %in% centres, "mnpctrname"])
})

# check dijunct selections
patients <- c("RPACK-USB-123")
centres <- c("461", "462")
sT_subset_disjunct <- subset_secuTrial(sT, patient = patients, centre = centres)

test_that("Subset disjunct patient and centre", {
  expect_equal(TRUE, compare_patients(sT_subset_disjunct, c()))
})
