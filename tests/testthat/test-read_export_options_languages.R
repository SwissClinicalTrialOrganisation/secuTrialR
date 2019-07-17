context("export options internationalization")

# ENGLISH
# english long file name
export_path_long_en <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                  package = "secuTrialR")
sT_export_long_en <- read_secuTrial_export(data_dir = export_path_long_en)
# english short file name
export_path_short_en <- system.file("extdata", "examples_short", "s_export_CSV-xls_CTU05_20190710-110042_en.zip",
                                  package = "secuTrialR")
sT_export_short_en <- read_secuTrial_export(data_dir = export_path_short_en)

# GERMAN
# german long file names
export_path_long_de <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref_german.zip",
                                  package = "secuTrialR")
sT_export_long_de <- read_secuTrial_export(data_dir = export_path_long_de)
# german short file
export_path_short_de <- system.file("extdata", "examples_short", "s_export_CSV-xls_CTU05_20190710-110208_de.zip",
                                    package = "secuTrialR")
sT_export_short_de <- read_secuTrial_export(data_dir = export_path_short_de)

# ITALIAN
# italian long file name
export_path_long_it <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref_italian.zip",
                                  package = "secuTrialR")
sT_export_long_it <- read_secuTrial_export(data_dir = export_path_long_it)
# italian short file name
export_path_short_it <- system.file("extdata", "examples_short", "s_export_CSV-xls_CTU05_20190710-105847_it.zip",
                                    package = "secuTrialR")
sT_export_short_it <- read_secuTrial_export(data_dir = export_path_short_it)

# FRENCH
# french long file name
export_path_long_fr <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref_french.zip",
                                   package = "secuTrialR")
sT_export_long_fr <- read_secuTrial_export(data_dir = export_path_long_fr)
# french short file name
export_path_short_fr <- system.file("extdata", "examples_short", "s_export_CSV-xls_CTU05_20190710-105945_fr.zip",
                                    package = "secuTrialR")
sT_export_short_fr <- read_secuTrial_export(data_dir = export_path_short_fr)

# SPANISH
# spanish long file name
export_path_long_es <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref_spanish.zip",
                                  package = "secuTrialR")
sT_export_long_es <- read_secuTrial_export(data_dir = export_path_long_es)
# spanish short file name
export_path_short_es <- system.file("extdata", "examples_short", "s_export_CSV-xls_CTU05_20190710-105648_es.zip",
                                    package = "secuTrialR")
sT_export_short_es <- read_secuTrial_export(data_dir = export_path_short_es)

# POLISH - not supported
# polish long file name
export_path_long_pl <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref_polish.zip",
                                  package = "secuTrialR")
sT_export_long_pl <- read_secuTrial_export(data_dir = export_path_long_pl)
# polish short file name
export_path_short_pl <- system.file("extdata", "examples_short", "s_export_CSV-xls_CTU05_20190710-105752_pl.zip",
                                    package = "secuTrialR")
sT_export_short_pl <- read_secuTrial_export(data_dir = export_path_short_pl)

# UNKNOWN
# unknown language short file name
export_path_short_unsupported <- system.file("extdata", "examples_short",
                                             "s_export_CSV-xls_CTU05_20190710-110208_unsupported.zip",
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

test_that("Unsuported language loads", {
  expect_error(read_secuTrial_export(data_dir = export_path_short_unsupported), unsupported_error, fixed = TRUE)
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
