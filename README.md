# secuTrialR ![travis](https://api.travis-ci.com/SwissClinicalTrialOrganisation/secuTrialR.svg?branch=master)

An R package to handle data from the clinical data management system (CDMS) [secuTrial](https://www.secutrial.com/en/).

## Installing from github with devtools

``` r
devtools::install_github("SwissClinicalTrialOrganisation/secuTrialR")
```

## Testing with devtools

``` R
# run tests
devtools::test("secuTrialR")
# spell check
devtools::spell_check("secuTrialR")
```

## Linting with lintr

```r
# lint the package
library(lintr)
lint_package("secuTrialR", linters = with_defaults(camel_case_linter = NULL,
                                                   object_usage_linter = NULL,
                                                   line_length_linter(125)))
```

## Guidelines for contributers

In order to contribute to this R package you should fork the main repository.
After you have made your changes please run the tests and lint your code as 
indicated above. If all
[tests](README.md#testing-with-devtools)
pass and linting confirms that your coding style
conforms you can send a pull request (PR).  
The PR should have a description to help the reviewer understand what has been 
added/changed. New functionalities must be thoroughly documented, have examples 
and should be accompanied by at least one [test](tests/testthat/) to ensure longterm 
robustness. The PR will only be reviewed if all travis checks are successful. 
The person sending the PR should not be the one merging it.
