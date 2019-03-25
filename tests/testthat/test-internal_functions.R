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
export_options <- load_export_options(data_dir = system.file("extdata",
                                                             "s_export_CSV-xls_BMD.zip",
                                                             package = "secuTrialR"))
# load casenodes table
casenodes <- load_export_table(data_dir = system.file("extdata",
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

# ---- test .move_column_after
test_that("pat_id moved after mnpaid column.", {
  expect_equal(names(secuTrialR:::.move_column_after(df = casenodes,
                                                     col_name = "mnppid",
                                                     col_name_after = "mnpaid"))[which(names(casenodes) == "mnpaid")],
               "mnppid")
})
