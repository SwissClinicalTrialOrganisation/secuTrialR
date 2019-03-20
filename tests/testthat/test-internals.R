context("internal functions testing")

# ---- test .constructmetaname

# init meta_names
meta_names <- list()
meta_names["forms"] <- "forms"
meta_names["casenodes"] <- "casenodes"
meta_names["centres"] <- "centres"
meta_names["items"] <- "items"
meta_names["questions"] <- "questions"
meta_names["visitplan"] <- "visitplan"
meta_names["visitplanforms"] <- "visitplanforms"
meta_names["cl"] <- "cl"

file_tag <- "_DEM00_20190318-104703"
file_extension <- "xls"
entry <- "items"

test_that("Meta name constructed.", {
  expect_equal(.constructmetaname("items", meta_names, file_tag, file_extension), "items_DEM00_20190318-104703.xls")
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
# load patient table
patient <- load_export_table(data_dir = system.file("extdata",
                                                    "s_export_CSV-xls_BMD.zip",
                                                    package = "secuTrialR"),
                             file_name = "cn.xls",
                             export_options = export_options)

# ---- test .move_column_to_pos
test_that("pat.id moved to index 2.", {
  expect_equal(names(secuTrialR:::.move_column_to_pos(df = patient, col_idx = 1, new_col_idx = 2))[2],
               "pat.id")
})

# ---- test .move_column_after
test_that("pat.id moved after mnpaid column.", {
  expect_equal(names(secuTrialR:::.move_column_after(df = patient,
                                                     col_name = "pat.id",
                                                     col_name_after = "mnpaid"))[which(names(patient) == "mnpaid")],
               "pat.id")
})
