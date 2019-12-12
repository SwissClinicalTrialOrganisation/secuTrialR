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
