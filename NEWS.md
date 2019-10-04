# secuTrialR 0.6.0
`read_secuTrial_raw()` and `read_secuTrial()` no longer fail due to missing Add-ID, centre information or project setup in export data. Instead, adding of `pat_id` (no Add-ID), `centre` (no centre information) and `visit_name` (no project setup) to the data tables are now omitted if the accoring data for the operation is not available.

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
