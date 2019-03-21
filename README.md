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
# lint file id_conversions.R assuming the secuTrialR directory is in your home
lintr::lint("~/secuTrialR/R/id_conversions.R")
# following lint returns can be ignored
# lines should not be more than 80 characters
# Variable and function names should be all lowercase
```

## Guidelines for contributers

In order to contribute to this R package you should fork the main repository.
After you have made your changes please run the tests and lint your code as 
indicated above. If all tests pass and linting confirms that your coding style
conforms you can send a pull request (PR).  
The PR should have a description to help the reviewer understand what has been 
added/changed. New functionalities must be thoroughly documented, have examples 
and should be accompanied by at least one [test](tests/testthat/) to ensure longterm 
robustness. The PR will only be reviewed if all travis checks are successful. 
The person sending the PR should not be the one merging it.

