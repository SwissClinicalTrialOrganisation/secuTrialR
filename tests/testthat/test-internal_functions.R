context("internal functions testing")

skip_on_cran()

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
  expect_equal(.construct_metaname("items", meta_names, file_tag, file_extension),
               "items_DEM00_20190318-104703.xls")
})


# ---- test .removeproj
file_name <- "mnpdem00bmd_DEM00_20190318-104703.xls"

test_that("File name truncated.", {
  expect_equal(.removeproj(file_name), "DEM00_20190318-104703.xls")
})

# load export options
export_options <- read_export_options(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                             "s_export_CSV-xls_BMD_short_en_utf8.zip",
                                                             package = "secuTrialR"))
# load casenodes table
casenodes <- read_export_table(data_dir = system.file("extdata", "sT_exports", "BMD",
                                                      "s_export_CSV-xls_BMD_short_en_utf8.zip",
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


# ---- test .get_dict for items table dictionary
dict_items <- .get_dict("dict_items_table.csv")

test_that("items table dictionary loaded", {
  expect_equal(dim(dict_items)[1], 6)
  expect_equal(dim(dict_items)[2], 8)
})


# ---- test .get_dict for export options keys dictionary
dict_export_keys <- .get_dict("dict_export_options_keys.csv")
dict_export_keys_de <- .get_dict("dict_export_options_keys.csv", "de")
dict_export_keys_en <- .get_dict("dict_export_options_keys.csv", "en")
dict_export_keys_fr <- .get_dict("dict_export_options_keys.csv", "fr")
dict_export_keys_it <- .get_dict("dict_export_options_keys.csv", "it")
dict_export_keys_es <- .get_dict("dict_export_options_keys.csv", "es")
dict_export_keys_pl <- .get_dict("dict_export_options_keys.csv", "pl")
dict_export_keys_unknown <- .get_dict("dict_export_options_keys.csv", "un")

test_that("items table dictionary loaded", {
  expect_equal(dim(dict_export_keys)[1], 6)
  expect_equal(dim(dict_export_keys)[2], 13)
  expect_equal(dict_export_keys_de$lang, "de")
  expect_equal(dict_export_keys_en$lang, "en")
  expect_equal(dict_export_keys_fr$lang, "fr")
  expect_equal(dict_export_keys_it$lang, "it")
  expect_equal(dict_export_keys_es$lang, "es")
  expect_equal(dict_export_keys_pl$lang, "pl")
  expect_equal(dim(dict_export_keys_unknown)[1], 0)
})

# ---- test .get_dict for export options settings dictionary
dict_export_settings <- .get_dict("dict_export_options_settings.csv")

test_that("items table dictionary loaded", {
  expect_equal(dim(dict_export_settings)[1], 6)
  expect_equal(dim(dict_export_settings)[2], 12)
})

# ---- test .get_export_language
parse_export_options <- function(data_dir) {
  files <- unzip(data_dir, list = TRUE)
  study_options_file_idx <- grep("ExportOptions", files$Name)
  file_con <- unz(data_dir, files$Name[study_options_file_idx])
  parsed_export <- readLines(file_con)
  close(file_con)
  return(parsed_export)
}
# load exports in supported languages
data_dirs_supported <- list(en = system.file("extdata", "sT_exports", "lnames",
                                             "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                                             package = "secuTrialR"),
                            de = system.file("extdata", "sT_exports", "lnames",
                                             "s_export_CSV-xls_CTU05_long_ref_miss_de_utf8.zip",
                                             package = "secuTrialR"),
                            fr = system.file("extdata", "sT_exports", "lnames",
                                             "s_export_CSV-xls_CTU05_long_ref_miss_fr_utf8.zip",
                                             package = "secuTrialR"),
                            it = system.file("extdata", "sT_exports", "lnames",
                                             "s_export_CSV-xls_CTU05_long_ref_miss_it_utf8.zip",
                                             package = "secuTrialR"),
                            es = system.file("extdata", "sT_exports", "lnames",
                                             "s_export_CSV-xls_CTU05_long_ref_miss_es_utf8.zip",
                                             package = "secuTrialR"),
                            pl = system.file("extdata", "sT_exports", "lnames",
                                             "s_export_CSV-xls_CTU05_long_ref_miss_pl_utf8.zip",
                                             package = "secuTrialR"))
parsed_exports_supported <- lapply(data_dirs_supported, function(x) parse_export_options(x))

test_that("export options language parsed", {
  expect_equal(.get_export_language(parsed_exports_supported[[1]]), "en")
  expect_equal(.get_export_language(parsed_exports_supported[[2]]), "de")
  expect_equal(.get_export_language(parsed_exports_supported[[3]]), "fr")
  expect_equal(.get_export_language(parsed_exports_supported[[4]]), "it")
  expect_equal(.get_export_language(parsed_exports_supported[[5]]), "es")
  expect_equal(.get_export_language(parsed_exports_supported[[6]]), "pl")
})

# unknown language short file name
data_dir_unsupported <- system.file("extdata", "sT_exports", "snames",
                                    "s_export_CSV-xls_CTU05_short_meta_ref_miss_unsupported_utf8.zip",
                                    package = "secuTrialR")
# unsupported language error msg
unsupported_error <- "Your export language is not supported and can not be processed."

test_that("Unsuported language load error", {
  expect_error(.get_export_language(parsed_export_unsupported, unsupported_error, fixed = TRUE))
})
