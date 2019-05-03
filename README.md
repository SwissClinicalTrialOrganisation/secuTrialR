
<!-- README.md is generated from README.Rmd. Please edit that file -->

# secuTrialR ![travis](https://api.travis-ci.com/SwissClinicalTrialOrganisation/secuTrialR.svg?branch=master)

An R package to handle data from the clinical data management system
(CDMS) [secuTrial](https://www.secutrial.com/en/).

## Installing from github with devtools

``` r
devtools::install_github("SwissClinicalTrialOrganisation/secuTrialR")
```

## Basic usage

Load the package

``` r
library(secuTrialR)
```

### Load the dataset

``` r
# prepare path to example export
export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- load_secuTrial_export(data_dir = export_location)
```

### Variable labels

For creating tables, it is often useful to have access to variable
labels. This is simple in secuTrialR.

``` r
labs <- labels_secuTrial(sT_export)
# query the list with the variable name of interest
labs[["age"]]
```

    ## [1] "Age"


### Prepare dates

Dates are a very common data type. They cannot be easily used though in
their export format. This is also easily done in
    secuTrialR:

``` r
dates <- dates_secuTrial(sT_export)
```

    ## Warning in dates_secuTrial.data.frame(tmp, datevars, format): no dates
    ## detected in bmd

    ## Warning in dates_secuTrial.data.frame(tmp, datevars, format): no dates
    ## detected in atbmd

There are no date variables in this dataset, hence the warnings.

### Linkages amongst forms

Linkages amongst forms can be explored with the `links_secuTrial`
function. This relies on the `igraph` package to create a network. The
network is plotted using `tikz`, which allows one to interact with the
network (e.g. move nodes around in order to read the label better). The
device ID is returned to the console, but can be ignored. Forms are
plotted in red, variables in blue.

``` r
links_secuTrial(sT_export)
```

![](inst/extdata/map.png)
<!-- Figure has to be generated outside of the Rmd file - resize the window and select view/"fit to screen", export it to a PDF and then convert it to a PNG -->


## For contributors

### Testing with devtools

``` r
# run tests
devtools::test("secuTrialR")
# spell check -> will contain some R and secuTrial specific words which is fine
devtools::spell_check("secuTrialR")
```

### Linting with lintr

``` r
# lint the package -> should be clean
library(lintr)
lint_package("secuTrialR", linters = with_defaults(camel_case_linter = NULL,
                                                   object_usage_linter = NULL,
                                                   line_length_linter(125)))
```

### Generating the README file

The README file contains both standard text and interpreted R code. It
must therefore be compiled. Changes should be made in the README.Rmd
file and the file “knited” with R. This is easiest with RStudio, but
other methods are available.

### Guidelines for contributers

In order to contribute to this R package you should fork the main
repository. After you have made your changes please run the
[tests](README.md#testing-with-devtools) and
[lint](README.md#linting-with-lintr) your code as indicated above. If
all tests pass and linting confirms that your coding style conforms you
can send a pull request (PR).  
The PR should have a description to help the reviewer understand what
has been added/changed. New functionalities must be thoroughly
documented, have examples and should be accompanied by at least one
[test](tests/testthat/) to ensure longterm robustness. The PR will only
be reviewed if all travis checks are successful. The person sending the
PR should not be the one merging it.

A depiction of the core functionalities for loading can be found
[here](inst/extdata/secuTrialR.png).
