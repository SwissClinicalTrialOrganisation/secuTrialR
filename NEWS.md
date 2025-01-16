# secuTrialR 1.3.4

* `read_export_options` gives a more useful error message if there are multiple ExportOptions files in the export (which is [a bug in secuTrial itself](https://www.secutrial.com/ticketsystem/view.php?id=12860), observed in at least some 6.4.1 versions, as well as [6.5.1.13](#268))
* `read_export` returns error messages from `read_export_options` rather than swallowing them, which should help users more than the previous error message.

# secuTrialR 1.3.2 and 1.3.3

* clear some notes and warnings in preparation for CRAN submission (#265)

# secuTrialR 1.3.1

* `build_secuTrial_url()` now has optional parameter `prefix` with default `"apps/WebObjects/"`. (#263)

# secuTrialR 1.3.0

* bug fix to read escaped enclosure characters in free text items, e.g. `\"` (#220, #261)
  * `read_export_options()` now also reads enclosure character from header
  * `.print.secuTrialoptions()` now also prints enclosure character. Quote and Tabulator are now printed as `'\"'` and `'\t'` rather than `'"'` and `'    '`. 
  * `read_export_table()` now uses `readr::read_delim()` with options `escape_backslash = TRUE` and `quote = export_options$quote`
* warning for ISO-8859 encoding moved from `read_export_table()` to `read_secuTrial_raw()` (one warning per export file instead of several warnings for each table)
* `read_export_options()` now reads only the first 10 lines to find the encoding. Reading all lines would fail if the export options were ISO-8859 encoded and contained 'problematic' characaters, e.g. in center names.
* Removed `encoding` option in `plot_recruitment()`: Not needed here, as centres are read with correct encoding in `read_secuTrial_raw()` and stored in the secuTrialR object.

# secuTrialR 1.2.0

* `write_secuTrial()` no longer allows `format = "sas"` due to deprecation of write_sas in `haven` (version 2.5.2).
* `read_secuTrial_raw` now produces a more explicit warning when ISO-8859 encoding is used. Recent changes in R seem to have reduced compatibility with this encoding.

# secuTrialR 1.1.1
* fix CRAN notes - remove codecov badge, check class via inherits

# secuTrialR 1.1.0
* warning regarding overwritten `pat_id` variable 

# secuTrialR 1.0.12
* adapt `as.data.frame.secuTrialData()` to allow named character vectors as option `data.frames` to specify custom names for data.frames (#250) 

# secuTrialR 1.0.11
* recommendation to export with short names (#238)
* addition of arguments in `read_export_table` etc allowing passing other options to `read.table` (#240)
* addition of `dictionary_secuTrial` function for easy access a data dictionary (#241)
* `formtablename` added to the output of `visit_structure` for better reference between the table names and labels (#242)
* addition of `export_date` and `export_datetime` for easier access to the export date (and time) (#246)

# secuTrialR 1.0.10
* adapt `read_export_options()` to work with exports from new secuTrial version (6.1.2.5 - 6.3.2.5) (#245)

# secuTrialR 1.0.9
* transferred the `Maintainer` tag to `Alan Haynes`

# secuTrialR 1.0.8
* reverted `factorize_secuTrial()` back to 1.0.3 version due to problems with lookup table factorization (#224)
* added citation (doi: 10.21105/joss.02816)

# secuTrialR 1.0.7
* moved `tcltk` and `igraph` dependency to suggested (#223)

# secuTrialR 1.0.6
* added `skip` parameter to `read_validation_overview()` (#212)

# secuTrialR 1.0.5
* fixed a bug where `subset_secuTrial()` would drop labels during the subsetting process (#203)

# secuTrialR 1.0.4
* improved import speed, specifically through changes in `dates_secutrial()` and `factorize_secutrial()` (#204)

# secuTrialR 1.0.1, 1.0.2, 1.0.3
* adjustments to handle review feedback from CRAN (#190)

# secuTrialR 1.0.0
* clarify correct options in `read_secuTrial()` failure message (#187)
* check for `project_setup` in `visit_structure()` (#181)

# secuTrialR 0.9.1
* added "Form meta data: Structure" export option information to `export_options` (#182)
* added error handling for missing structure data when running `annual_recruitment()` and `return_random_participants()` (#182)

# secuTrialR 0.9.0
* restructuring in preparation for a release on CRAN

# secuTrialR 0.8.9
* added suggestion to *NOT* export form data of hidden fields (#177)

# secuTrialR 0.8.8
* added check to make sure that specified centres are part of the export in `return_random_participants()` (#151)

# secuTrialR 0.8.7
* extended failure comment in `read_secuTrial()` to indicate that the problem could be a rectangular export file (#168)
* added "Form data of hidden fields" export option information to `export_options` (#171)
* added `return_hidden_items()` function (#172)

# secuTrialR 0.8.6
* bug fix: presence of the audit trail was incorrectly identified due to a comment in the source file of the export options (see #155, comments from @suvi-subra and @OliviaEbnerIAS)

# secuTrialR 0.8.5
* Added sorting option to visit_structure. (#152)

# secuTrialR 0.8.4
* adjusted warning message in `label_secuTrial()`
* only allow unique labels in `label_secuTrial()`
* added "Frequent warning messages" paragraph to the vignette (#156)

# secuTrialR 0.8.3
* added up-to-date vignette (#99)
* path in `print.secuTrialdata` is now wrapped at 80 characters

# secuTrialR 0.8.2
* `secutrialoptions` class is now `secuTrialoptions`.

# secuTrialR 0.8.1
* add appveyor testing, pkgdown site
* fix possible bug on windows due to regex in .prep_line_items (used in plot_recruitment) (#147)

# secuTrialR 0.8.0
* Changed license for the package from GPL-2 to MIT.

# secuTrialR 0.7.9
* The general nomenclature for a study subject will from now on be participant (pat). All variations of this
(e.g. case, patient) have been adjusted in the code and the documentation.

# secuTrialR 0.7.8
* Removed generic `plot()` function for `secuTrialdata` objects. (#139)

# secuTrialR 0.7.7
* `read_secuTrial()` and `read_secuTrial_raw()` now check if the input file exists. (#137)

# secuTrialR 0.7.6
* `factorize_secuTrial()` warning messages have been adjusted to improve trouble shooting experience. (#134, #135)

# secuTrialR 0.7.5
* `dates_secuTrial()` incomplete date warnings are now concatenated and returned as one warning per form instead of many. (#124)

# secuTrialR 0.7.4
* Fixed issue #121 on GitHub. `factorize_secuTrial()` can now handle exports which have the reset option
enabled in radio buttons.

# secuTrialR 0.7.3
* `write_secuTrial()` now allows xpt version 8 files to be written. (closes #57)

# secuTrialR 0.7.2
* `check_export_options()` function was added. It informs on deviations from suggested export options. (closes #17)
* Removed tracking of obsolete export options (`partial_date_string`, `partial_date_handling`, `unknown_date_string`).
* Added `format_info` (e.g. "CSV format for MS Excel") to `export_options`.

# secuTrialR 0.7.1
* Fixed issue #116 on GitHub.

# secuTrialR 0.7.0
* `subset_secuTrial()` function was added. It allows subsetting of secuTrialdata based on patient ID and/or study centre name.
* `get_participants()` function was added. It allows easy extractions of participant info from a secuTrialdata object.

# secuTrialR 0.6.5
* `return_random_cases()` now returns a list. The first element are the cases and the second element is the output of `RNGkind()`.

# secuTrialR 0.6.4
* New function `diff_secuTrial()` added to allow light weight comparison of the setup of two secuTrial exports.

# secuTrialR 0.6.3
* Metadata variables are now also transformed to date and datetime formats, whenever appropriate.

# secuTrialR 0.6.2
* `factorize_secuTrial()` now no longer triggers an unexpected warning when the name of a secuTrial lookuptable is equal to the name of the variable it is being used in. (PR #108)

# secuTrialR 0.6.1
* `return_random_cases()` has been added to the package. It allows to sample a random subset of cases from a secuTrial export in a reproducible fashion.

# secuTrialR 0.6.0
* `read_secuTrial_raw()` and `read_secuTrial()` no longer fail due to missing Add-ID, centre information or project setup in export data. Instead, adding of `pat_id` (no Add-ID), `centre` (no centre information) and `visit_name` (no project setup) to the data tables is now omitted if the relevant data for the operation is not available.

# secuTrialR 0.5.5
* `read_secuTrial_raw()` and `read_secuTrial()` no longer fail due to missing "Description" in export options.

# secuTrialR 0.5.4
* `dates_secuTrial()` now warns if not all dates were parsed (expected if there are incomplete dates).
* `factorize_secuTrial()` now warns if there are issues with the factorization (not expected to trigger).

# secuTrialR 0.5.2
* New function `build_secuTrial_url()` has been added. It allows users to easily compose URLs to specific secuTrial forms.

# secuTrialR 0.5.0
* The function name of `read_secuTrial_export()` has been changed to `read_secuTrial_raw()`
  to avoid confusion with `read_secuTrial()`.
 
# secuTrialR 0.4.16
* As of version 0.4.17, changes will be recorded in the NEWS file.
