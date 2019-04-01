# secuTrialR ![travis](https://api.travis-ci.com/SwissClinicalTrialOrganisation/secuTrialR.svg?branch=master)

An R package to handle data from the clinical data management system (CDMS) [secuTrial](https://www.secutrial.com/en/).

## Installing from github with devtools

``` r
devtools::install_github("SwissClinicalTrialOrganisation/secuTrialR")
```

## Basic usage

``` r
# prepare path to example export
export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- load_secuTrial_export(data_dir = export_location)
# basic info on the dataset
sT_export

```


## For contributors
### Testing with devtools

``` R
# run tests
devtools::test("secuTrialR")
# spell check -> will contain some R and secuTrial specific words which is fine
devtools::spell_check("secuTrialR")
```

### Linting with lintr

```r
# lint the package -> should be clean
library(lintr)
lint_package("secuTrialR", linters = with_defaults(camel_case_linter = NULL,
                                                   object_usage_linter = NULL,
                                                   line_length_linter(125)))
```

### Guidelines for contributers

In order to contribute to this R package you should fork the main repository.
After you have made your changes please run the 
[tests](README.md#testing-with-devtools)
and 
[lint](README.md#linting-with-lintr) your code as 
indicated above. If all tests pass and linting confirms that your 
coding style conforms you can send a pull request (PR).  
The PR should have a description to help the reviewer understand what has been 
added/changed. New functionalities must be thoroughly documented, have examples 
and should be accompanied by at least one [test](tests/testthat/) to ensure longterm 
robustness. The PR will only be reviewed if all travis checks are successful. 
The person sending the PR should not be the one merging it.

A depiction of the core functionalities for loading can be found [here](inst/extdata/secuTrialR.png).
