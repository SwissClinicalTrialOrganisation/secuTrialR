
<!-- README.md is generated from README.Rmd. Please edit that file -->

# secuTrialR ![travis](https://api.travis-ci.com/SwissClinicalTrialOrganisation/secuTrialR.svg?branch=master)

An R package to handle data from the clinical data management system
(CDMS) [secuTrial](https://www.secutrial.com/en/).

## Installing from github with devtools

``` r
devtools::install_github("SwissClinicalTrialOrganisation/secuTrialR")
```

## Basic usage

``` r
library(secuTrialR)
```

``` r
# prepare path to example export
export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
                               package = "secuTrialR")
# load all export data
sT_export <- load_secuTrial_export(data_dir = export_location)
```

Printing the object gives some useful information about the objects
within the objects, such as the original file name in the datafile, it’s
name in the `secuTrialdata` object, together with the number of rows and
columns and a column indicating whether the object is metadata or
    not:

``` r
sT_export
```

    ## SecuTrial data imported from C:/R/R-3.4.2/library/secuTrialR/extdata/s_export_CSV-xls_BMD.zip 
    ##  table nrow ncol  meta original_name
    ##     vp    1   10  TRUE        vp.xls
    ##   vpfs    1    2  TRUE      vpfs.xls
    ##     fs    1    7  TRUE        fs.xls
    ##     qs    1    7  TRUE        qs.xls
    ##     is    3    8  TRUE        is.xls
    ##    ctr    1    3  TRUE       ctr.xls
    ##     cn  113   13  TRUE        cn.xls
    ##   atcn    0    6  TRUE      atcn.xls
    ##  atcvp    0   11  TRUE     atcvp.xls
    ##    qac    0   10  TRUE       qac.xls
    ##    cts    0    8  TRUE       cts.xls
    ##    bmd  504   27 FALSE       bmd.xls
    ##  atbmd    0   28 FALSE     atbmd.xls

For creating tables, it is often useful to have access to variable
labels. This is simple in secuTrialR.

``` r
labs <- labels_secuTrial(sT_export)
# query the list with the variable name of interest
labs[["age"]]
```

    ## [1] "Age"

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
