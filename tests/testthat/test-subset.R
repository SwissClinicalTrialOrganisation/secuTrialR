context("subset")

skip_on_cran()

## compares two secutrialdata for equality. very specialized function for the purpose of testing.
## if equal, TRUE is returned, otherwise, FALSE.
## Equality parameters:
## "atr" - equality of attributes in all elements of dat_ref and dat
## "dat" - equality of data contained in all elements of dat_ref and dat
## "all" - equality of data and attributes
secuTrial_is_equal <- function(dat_ref, dat, equality = "all") {
  if (!equality %in% c("all", "attr", "data")) stop("eqaulity must be all, attr or data")
  forms_equal <- TRUE
  attrs_equal <- TRUE
  for (tab in names(dat)) {
    if (tab == "export_options") {
      next
    }
    attrs_equal <- suppressWarnings(forms_equal & all_equal(sort(names(attributes(dat[[tab]]))),
                                                            sort(names(attributes(dat_ref[[tab]])))))
    forms_equal <- suppressWarnings(attrs_equal & all_equal(dat[[tab]], dat_ref[[tab]]))
    if (equality == "all") {
      return(forms_equal & attrs_equal)
    } else if (equality == "attr") {
      return(attr_equal)
    } else {
      return(forms_equal)
    }
  }
}

## function that compares participant IDs present in all data frames
## of a "dat" secuTrialdata object with those provided via "participants" character vector
compare_participants <- function(dat, participants) {
  pats_equal <- TRUE
  for (tab in names(dat)) {
    if (tab == "export_options") {
      next
    }
    if (any(names(dat[[tab]]) %in% "pat_id") & nrow(dat[[tab]]) > 0) {
      pats_equal <- pats_equal & all(dat[[tab]][["pat_id"]] %in% participants)
    } else if (any(names(dat[[tab]]) %in% "mnpaid") & nrow(dat[[tab]]) > 0) {
      pats_equal <- pats_equal & all(dat[[tab]][["mnpaid"]] %in% participants)
    } else{
      next
    }
    if (!pats_equal) {
      print(unique(dat[[tab]] %>% select(contains("pat_id"), contains("mnpaid"))))
      return(pats_equal)
    }
  }
  return(pats_equal)
}


sT <- read_secuTrial(system.file("extdata", "sT_exports", "exp_opt",
                                 "s_export_CSV-xls_CTU05_all_info.zip",
                                 package = "secuTrialR"))

sT_InselUSB <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                          "s_export_CSV-xls_CTU05_short_Insel-USB_en.zip",
                                          package = "secuTrialR"))

sT_noid <- read_secuTrial(system.file("extdata", "sT_exports", "exp_opt",
                                      "s_export_CSV-xls_CTU05_no_addid.zip",
                                      package = "secuTrialR"))

sT_nocentre <- read_secuTrial(system.file("extdata", "sT_exports", "exp_opt",
                                          "s_export_CSV-xls_CTU05_no_centre_info.zip",
                                          package = "secuTrialR"))

sT_long <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                      "s_export_CSV-xls_CTU05_long_all-info_en.zip",
                                      package = "secuTrialR"))

sT_InselUSB_long <- read_secuTrial(system.file("extdata", "sT_exports", "subset",
                                               "s_export_CSV-xls_CTU05_long_Insel-USB_en.zip",
                                               package = "secuTrialR"))

test_that("Subset errors", {
  expect_error(subset_secuTrial(data.frame()))
  expect_error(subset_secuTrial(sT_noid, participant = "1"))
  expect_error(subset_secuTrial(sT_nocentre, centre = "1"))
})

## test subsetting based on centers
centres <- c("Inselspital Bern (RPACK)", "Universitätsspital Basel (RPACK)")
sT_subset <- subset_secuTrial(sT, centre = centres)
sT_subset_exclude <- subset_secuTrial(sT, centre = "Charité Berlin (RPACK)", exclude = TRUE)
sT_subset_long <- subset_secuTrial(sT_long, centre = centres)

test_that("Subset centre", {
  expect_equal(sT, subset_secuTrial(sT))
  expect_equal(sT_long, subset_secuTrial(sT_long))
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB, sT_subset, "all"))
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB, sT_subset_exclude, "all"))
  expect_equal(TRUE, secuTrial_is_equal(sT_InselUSB_long, sT_subset_long, "all"))
})

## test subsetting based on participants
participants <- c("RPACK-CBE-001", "RPACK-CBE-002", "RPACK-CBE-003", "RPACK-CBE-004",
              "RPACK-CBE-005", "RPACK-INS-014", "RPACK-INS-015")
sT_subset <- subset_secuTrial(sT, participant = participants)
sT_subset_exclude <- subset_secuTrial(sT, participant = participants, exclude = TRUE)

test_that("Subset participant", {
  expect_equal(TRUE, all(unique(sT_subset$baseline$pat_id) %in% participants))
  expect_equal(TRUE, all(participants %in% unique(sT_subset$baseline$pat_id)))
  expect_equal(TRUE, compare_participants(sT_subset, participants))
  expect_equal(TRUE, compare_participants(sT_subset_exclude, sT$cn$mnpaid[!sT$cn$mnpaid %in% participants]))
  expect_equal(levels(sT$baseline$centre), levels(sT_subset$baseline$centre)) # center levels should stay same
  expect_equal(FALSE, all(sT$baseline$centre %in% sT_subset$baseline$centre)) # actually represented levels change
  expect_equal(TRUE, all(sT$ctr$mnpctrid %in% sT_subset$ctr$mnpctrid)) # all centres should be present
})

## test subsetting based on both participants and centres in one go
participants <- c("RPACK-CBE-001", "RPACK-CBE-002", "RPACK-CBE-003", "RPACK-CBE-004",
              "RPACK-CBE-005", "RPACK-INS-014", "RPACK-INS-015")
centres <- c("Charité Berlin (RPACK)", "Universitätsspital Basel (RPACK)")
sT_subset <- subset_secuTrial(sT, participant = participants, centre = centres)
sT_subset_exclude <- subset_secuTrial(sT, participant = participants, centre = centres, exclude = TRUE)
centres_id <- sT$ctr$mnpctrid[sT$ctr$mnpctrname %in% centres]

test_that("Subset participant and centre", {
  expect_equal(TRUE, compare_participants(sT_subset,
                                      sT$cn[sT$cn[["mnpaid"]] %in% participants & sT$cn$mnpctrid %in% centres_id, "mnpaid"]))
  expect_equal(TRUE, all(sT_subset$ctr$mnpctrname %in% centres))
  expect_equal(levels(sT_subset$baseline$centre),  sT$ctr[sT$ctr$mnpctrname %in% centres, "mnpctrname"])

  expect_equal(TRUE, compare_participants(sT_subset_exclude, sT$cn[!sT$cn[["mnpaid"]] %in% participants &
                                                                 !sT$cn[["mnpctrid"]] %in% centres_id, "mnpaid"]))
  expect_equal(TRUE, all(!sT_subset_exclude$ctr$mnpctrname %in% centres))
  expect_equal(levels(sT_subset_exclude$baseline$centre), sT$ctr[!sT$ctr$mnpctrname %in% centres, "mnpctrname"])
})

# check subsetting disjunct sets of participants and centres
participants <- c("RPACK-USB-123")
centres <- c("Inselspital Bern (RPACK)", "Charité Berlin (RPACK)")
sT_subset_disjunct <- subset_secuTrial(sT, participant = participants, centre = centres)

sT_subset_nopat <- subset_secuTrial(sT, participant = "not a participant")
sT_subset_nocentre <- subset_secuTrial(sT, centre = "not a centre")
sT_subset_nopat_exclude <- subset_secuTrial(sT, participant = "not a participant", exclude = TRUE)
sT_subset_nocentre_exclude <- subset_secuTrial(sT, centre = "not a centre", exclude = TRUE)

test_that("Subset disjunct participant and centre", {
  expect_equal(TRUE, compare_participants(sT_subset_nopat, c()))
  expect_equal(TRUE, compare_participants(sT_subset_nocentre, c()))
  expect_equal(TRUE, secuTrial_is_equal(sT, sT_subset_nopat_exclude))
  expect_equal(TRUE, secuTrial_is_equal(sT, sT_subset_nocentre_exclude))
  expect_equal(TRUE, compare_participants(sT_subset_disjunct, c()))
})
