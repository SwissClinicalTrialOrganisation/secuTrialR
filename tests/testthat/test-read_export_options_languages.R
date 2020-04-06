context("export options internationalization")

skip_on_cran()

# ENGLISH
# english long file name
export_path_long_en <- system.file("extdata", "sT_exports", "lnames",
                                   "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                                  package = "secuTrialR")
sT_export_long_en <- read_secuTrial_raw(data_dir = export_path_long_en)
# english short file name
export_path_short_en <- system.file("extdata", "sT_exports", "snames",
                                    "s_export_CSV-xls_CTU05_short_meta_ref_miss_en_utf8.zip",
                                  package = "secuTrialR")
sT_export_short_en <- read_secuTrial_raw(data_dir = export_path_short_en)

# GERMAN
# german long file names
export_path_long_de <- system.file("extdata", "sT_exports", "lnames",
                                   "s_export_CSV-xls_CTU05_long_ref_miss_de_utf8.zip",
                                  package = "secuTrialR")
sT_export_long_de <- read_secuTrial_raw(data_dir = export_path_long_de)
# german short file
export_path_short_de <- system.file("extdata", "sT_exports", "snames",
                                    "s_export_CSV-xls_CTU05_short_meta_ref_miss_de_utf8.zip",
                                    package = "secuTrialR")
sT_export_short_de <- read_secuTrial_raw(data_dir = export_path_short_de)

# ITALIAN
# italian long file name
export_path_long_it <- system.file("extdata", "sT_exports", "lnames",
                                   "s_export_CSV-xls_CTU05_long_ref_miss_it_utf8.zip",
                                  package = "secuTrialR")
sT_export_long_it <- read_secuTrial_raw(data_dir = export_path_long_it)
# italian short file name
export_path_short_it <- system.file("extdata", "sT_exports", "snames",
                                    "s_export_CSV-xls_CTU05_short_ref_miss_it_utf8.zip",
                                    package = "secuTrialR")
sT_export_short_it <- read_secuTrial_raw(data_dir = export_path_short_it)

# FRENCH
# french long file name
export_path_long_fr <- system.file("extdata", "sT_exports", "lnames",
                                   "s_export_CSV-xls_CTU05_long_ref_miss_fr_utf8.zip",
                                   package = "secuTrialR")
sT_export_long_fr <- read_secuTrial_raw(data_dir = export_path_long_fr)
# french short file name
export_path_short_fr <- system.file("extdata", "sT_exports", "snames",
                                    "s_export_CSV-xls_CTU05_short_meta_ref_miss_fr_utf8.zip",
                                    package = "secuTrialR")
sT_export_short_fr <- read_secuTrial_raw(data_dir = export_path_short_fr)

# SPANISH
# spanish long file name
export_path_long_es <- system.file("extdata", "sT_exports", "lnames",
                                   "s_export_CSV-xls_CTU05_long_ref_miss_es_utf8.zip",
                                  package = "secuTrialR")
sT_export_long_es <- read_secuTrial_raw(data_dir = export_path_long_es)
# spanish short file name
export_path_short_es <- system.file("extdata", "sT_exports", "snames",
                                    "s_export_CSV-xls_CTU05_short_meta_ref_miss_es_utf8.zip",
                                    package = "secuTrialR")
sT_export_short_es <- read_secuTrial_raw(data_dir = export_path_short_es)

# POLISH - not supported
# polish long file name
export_path_long_pl <- system.file("extdata", "sT_exports", "lnames",
                                   "s_export_CSV-xls_CTU05_long_ref_miss_pl_utf8.zip",
                                  package = "secuTrialR")
sT_export_long_pl <- read_secuTrial_raw(data_dir = export_path_long_pl)
# polish short file name
export_path_short_pl <- system.file("extdata", "sT_exports", "snames",
                                    "s_export_CSV-xls_CTU05_short_meta_ref_miss_pl_utf8.zip",
                                    package = "secuTrialR")
sT_export_short_pl <- read_secuTrial_raw(data_dir = export_path_short_pl)

# UNKNOWN
# unknown language short file name
export_path_short_unsupported <- system.file("extdata", "sT_exports", "snames",
                                             "s_export_CSV-xls_CTU05_short_meta_ref_miss_unsup_utf8.zip",
                                             package = "secuTrialR")

# unsupported language error msg
unsupported_error <- "Your export language is not supported and can not be processed."

# TESTS

test_that("Export language parsing", {
  expect_equal(sT_export_long_en$export_options$dict_items$lang, "en")
  expect_equal(sT_export_long_de$export_options$dict_items$lang, "de")
  expect_equal(sT_export_long_fr$export_options$dict_items$lang, "fr")
  expect_equal(sT_export_long_it$export_options$dict_items$lang, "it")
  expect_equal(sT_export_long_es$export_options$dict_items$lang, "es")
  expect_equal(sT_export_long_pl$export_options$dict_items$lang, "pl")
})

test_that("Unsupported language loads", {
  expect_error(read_secuTrial_raw(data_dir = export_path_short_unsupported), unsupported_error, fixed = TRUE)
})

test_that("Audit trail option parsing", {
  expect_true(sT_export_long_en$export_options$audit_trail)
  expect_true(sT_export_long_de$export_options$audit_trail)
  expect_true(sT_export_long_it$export_options$audit_trail)
  expect_true(sT_export_long_fr$export_options$audit_trail)
  expect_true(sT_export_long_es$export_options$audit_trail)
  expect_true(sT_export_long_pl$export_options$audit_trail)
})

test_that("Short names option parsing", {
  expect_false(sT_export_long_en$export_options$short_names)
  expect_false(sT_export_long_de$export_options$short_names)
  expect_false(sT_export_long_it$export_options$short_names)
  expect_false(sT_export_long_fr$export_options$short_names)
  expect_false(sT_export_long_es$export_options$short_names)
  expect_false(sT_export_long_pl$export_options$short_names)
  expect_true(sT_export_short_en$export_options$short_names)
  expect_true(sT_export_short_de$export_options$short_names)
  expect_true(sT_export_short_it$export_options$short_names)
  expect_true(sT_export_short_fr$export_options$short_names)
  expect_true(sT_export_short_es$export_options$short_names)
  expect_true(sT_export_short_pl$export_options$short_names)
})


test_that("Column names option parsing", {
  expect_true(sT_export_long_en$export_options$column_names)
  expect_true(sT_export_long_de$export_options$column_names)
  expect_true(sT_export_long_it$export_options$column_names)
  expect_true(sT_export_long_fr$export_options$column_names)
  expect_true(sT_export_long_es$export_options$column_names)
  expect_true(sT_export_long_pl$export_options$column_names)
})

test_that("Duplicate metadata option parsing", {
  expect_false(sT_export_long_en$export_options$duplicate_meta)
  expect_false(sT_export_long_de$export_options$duplicate_meta)
  expect_false(sT_export_long_it$export_options$duplicate_meta)
  expect_false(sT_export_long_fr$export_options$duplicate_meta)
  expect_false(sT_export_long_es$export_options$duplicate_meta)
  expect_false(sT_export_long_pl$export_options$duplicate_meta)
  expect_true(sT_export_short_de$export_options$duplicate_meta)
  expect_true(sT_export_short_it$export_options$duplicate_meta)
  expect_true(sT_export_short_fr$export_options$duplicate_meta)
  expect_true(sT_export_short_es$export_options$duplicate_meta)
  expect_true(sT_export_short_pl$export_options$duplicate_meta)
})

test_that("Separate reference table option parsing", {
  expect_true(sT_export_long_en$export_options$refvals_separate)
  expect_true(sT_export_long_de$export_options$refvals_separate)
  expect_true(sT_export_long_it$export_options$refvals_separate)
  expect_true(sT_export_long_fr$export_options$refvals_separate)
  expect_true(sT_export_long_es$export_options$refvals_separate)
  expect_true(sT_export_long_pl$export_options$refvals_separate)
})

test_that("Hidden fields option parsing", {
  expect_true(sT_export_long_en$export_options$hidden_fields)
  expect_true(sT_export_long_de$export_options$hidden_fields)
  expect_true(sT_export_long_it$export_options$hidden_fields)
  expect_true(sT_export_long_fr$export_options$hidden_fields)
  expect_true(sT_export_long_es$export_options$hidden_fields)
  expect_true(sT_export_long_pl$export_options$hidden_fields)
})

test_that("Form meta data Structure option parsing", {
  expect_true(sT_export_long_en$export_options$structure)
  expect_true(sT_export_long_de$export_options$structure)
  expect_true(sT_export_long_it$export_options$structure)
  expect_true(sT_export_long_fr$export_options$structure)
  expect_true(sT_export_long_es$export_options$structure)
  expect_true(sT_export_long_pl$export_options$structure)
})
