context("internal functions testing")

# ---- test .construct_metaname

# init meta_names
meta_names <- list()
meta_names["forms"] <- "forms"
meta_names["casenodes"] <- "casenodes"
meta_names["centres"] <- "centres"
meta_names["items"] <- "items"
meta_names["questions"] <- "questions"
meta_names["queries"] <- "queries"
meta_names["visitplan"] <- "visitplan"
meta_names["visitplanforms"] <- "visitplanforms"
meta_names["atcasenodes"] <- "atcasenodes"
meta_names["atcasevisitplans"] <- "atcasevisitplans"
meta_names["comments"] <- "comments"
meta_names["miv"] <- "miv"
meta_names["cl"] <- "cl"

file_tag <- "_DEM00_20190318-104703"
file_extension <- "xls"
entry <- "items"

test_that("Meta name constructed.", {
  expect_equal(.construct_metaname("items", meta_names, file_tag, file_extension), "items_DEM00_20190318-104703.xls")
})


# ---- test .removeproj
file_name <- "mnpdem00bmd_DEM00_20190318-104703.xls"

test_that("File name truncated.", {
  expect_equal(.removeproj(file_name), "DEM00_20190318-104703.xls")
})

# load export options
export_options <- read_export_options(data_dir = system.file("extdata",
                                                             "s_export_CSV-xls_BMD.zip",
                                                             package = "secuTrialR"))
# load casenodes table
casenodes <- read_export_table(data_dir = system.file("extdata",
                                                    "s_export_CSV-xls_BMD.zip",
                                                    package = "secuTrialR"),
                             file_name = "cn.xls",
                             export_options = export_options,
                             is_meta_table = TRUE)

# ---- test .move_column_to_pos
test_that("pat_id moved to index 2.", {
  expect_equal(names(secuTrialR:::.move_column_to_pos(df = casenodes, col_idx = 1, new_col_idx = 2))[2],
               "mnppid")
})

test_that("not a data.frame error",
          expect_error(secuTrialR:::.move_column_to_pos(c(1:3), 1, 2)))

test_that("not an integer error", {
          expect_error(secuTrialR:::.move_column_to_pos(
            data.frame(a = 1:3,
                       b = letters[1:3],
                       c = LETTERS[1:3]), 1.1, 2))
          expect_error(secuTrialR:::.move_column_to_pos(
            data.frame(a = 1:3,
                       b = letters[1:3],
                       c = LETTERS[1:3]), 1, 2.1))
  })

test_that("out of bounds error", {
          expect_error(secuTrialR:::.move_column_to_pos(
            data.frame(a = 1:3,
                       b = letters[1:3],
                       c = LETTERS[1:3]), 1, 5))
          expect_error(secuTrialR:::.move_column_to_pos(
            data.frame(a = 1:3,
                       b = letters[1:3],
                       c = LETTERS[1:3]), 13, 2))
  })

test_that("move to end", {
          expect_equal(names(secuTrialR:::.move_column_to_pos(
            data.frame(a = 1:3,
                       b = letters[1:3],
                       c = LETTERS[1:3]), 1, 3)), c("b", "c", "a"))
  })

# ---- test .move_column_after
test_that("pat_id moved after mnpaid column.", {
  expect_equal(names(secuTrialR:::.move_column_after(df = casenodes,
                                                     col_name = "mnppid",
                                                     col_name_after = "mnpaid"))[which(names(casenodes) == "mnpaid")],
               "mnppid")
})
test_that("unknown column error", {
  expect_error(secuTrialR:::.move_column_after(
    data.frame(a = 1:3,
               b = letters[1:3],
               c = LETTERS[1:3]), "d", "a"))
  expect_error(secuTrialR:::.move_column_after(
    data.frame(a = 1:3,
               b = letters[1:3],
               c = LETTERS[1:3]), "b", "d"))
})
test_that("reference column error", {
  expect_error(secuTrialR:::.move_column_after(
    data.frame(a = 1:3,
               b = letters[1:3],
               c = LETTERS[1:3]), "a", "a"))
})
test_that("not a data.frame error",
          expect_error(secuTrialR:::.move_column_after(c(1:3), "1", "2")))


# ---- test .get_items_dict
dict_items <- .get_items_dict()

test_that("items table dictionary loaded", {
  expect_equal(dim(dict_items)[1], 6)
  expect_equal(dim(dict_items)[2], 8)
})


# ---- test .get_export_keys_dict
dict_export_keys <- .get_export_keys_dict()

test_that("items table dictionary loaded", {
  expect_equal(dim(dict_export_keys)[1], 6)
  expect_equal(dim(dict_export_keys)[2], 13)
})


# ---- test .get_export_settings_dict
dict_export_settings <- .get_export_settings_dict()

test_that("items table dictionary loaded", {
  expect_equal(dim(dict_export_settings)[1], 6)
  expect_equal(dim(dict_export_settings)[2], 7)
})

# ---- test .get_export_language
data_dir <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref_german.zip",
                                   package = "secuTrialR")
files <- unzip(data_dir, list = TRUE)
study_options_file_idx <- grep("ExportOptions", files$Name)
file_con <- unz(data_dir, files$Name[study_options_file_idx])
parsed_export <- readLines(file_con)
close(file_con)

test_that("items table dictionary loaded", {
  expect_equal(.get_export_language(parsed_export), "de")
})
