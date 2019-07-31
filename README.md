---
output: github_document
---
<!-- README.md is generated from README.Rmd. Please edit that file -->


# secuTrialR ![travis](https://api.travis-ci.com/SwissClinicalTrialOrganisation/secuTrialR.svg?branch=master) [![codecov](https://codecov.io/github/SwissClinicalTrialOrganisation/secuTrialR/branch/master/graphs/badge.svg)](https://codecov.io/github/SwissClinicalTrialOrganisation/secuTrialR)

An R package to handle data from the clinical data management system (CDMS) [secuTrial](https://www.secutrial.com/en/).

## Installing from github with devtools


```r
devtools::install_github("SwissClinicalTrialOrganisation/secuTrialR")
```

## Basic usage
Load the package

```r
library(secuTrialR)
```
Load a dataset 

```r
export_location <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                               package = "secuTrialR")
ctu05 <- read_secuTrial(export_location)
```
This will load all sheets from the export into an object of class `secuTrialdata`, which is basically a list. It will always contain `export_details` (which are parsed from the HTML ExportOptions file that secuTrial generates). By default, it will also contain all other files in the dataset. secuTrialR automatically strips file names of dates. The new file names can be seen via `ctu05$export_options$data_names`. The function also adds [labels to variables](#variable-labels) and data.frames, converts [categorical variables to `factor`s](#prepare-factors) and ensures that [dates are `Date`s and date-times are `POSIXct`](#prepare-dates).
`read_secuTrial` is a wrapper for the functions described below, so it is possible to achieve more flexibility by using the individual functions (if necessary).
Individual tables can be extracted from the `ctu05` object via `tab <- ctu05$tab`, where `tab` is the table of interest.

<details><summary>Wrapped functions</summary>


#### Load the dataset

```r
# prepare path to example export
export_location <- system.file("extdata", "s_export_CSV-xls_BMD.zip",
                               package = "secuTrialR")
# load all export data
bmd_export <- read_secuTrial_export(data_dir = export_location)

# load a second dataset
export_location <- system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                               package = "secuTrialR")
ctu05_raw <- read_secuTrial_export(export_location)

# View names of the bmd_export object
names(bmd_export)
```

```
##  [1] "export_options" "fs"             "cn"             "ctr"           
##  [5] "is"             "qs"             "qac"            "vp"            
##  [9] "vpfs"           "atcn"           "atcvp"          "cts"           
## [13] "bmd"            "atbmd"
```

`read_secuTrial_export` returns an object of class `secuTrialdata`, which is basically a list. It will always contain `export_details` (which are parsed from the HTML ExportOptions file that secuTrial generates). By default, it will also contain all other files in the dataset. secuTrialR automatically strips file names of dates. The new file names can be seen via `bmd_export$export_options$data_names`.
<!-- DEDICATED ACCESSOR FUNCTION FOR DATA_NAMES? might already be implemented in the print method -->

`bmd_export` is a list, with class `secuTrialdata`. To prevent it from printing all data to the console, a special print method returns some useful information about the objects within `bmd_export` instead. The information returned includes the original file name in the datafile, it's name in the `secuTrialdata` object, together with the number of rows and columns and a column indicating whether the object is metadata or not:

```r
bmd_export
```

```
## SecuTrial data imported from /home/wrightp/R/x86_64-pc-linux-gnu-library/3.4/secuTrialR/extdata/s_export_CSV-xls_BMD.zip 
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
```

Individual tables can be extracted from the `bmd_export` object via `tab <- bmd_export$tab`, where `tab` is the table of interest.
<!-- accessor function? -->


#### Variable labels
For creating tables, it is often useful to have access to variable labels. secuTrialR supports two main methods for handling them - a named list, or via variable attributes. The list approach works as follows.

```r
labs <- labels_secuTrial(bmd_export)
# query the list with the variable name of interest
labs[["age"]]
```

```
## [1] "Age"
```

The attribute based approach adds labels as an attribute to a variable, which can then be accessed via `label(var)`.

```r
labelled <- label_secuTrial(bmd_export)
label(labelled$bmd$age)
```

```
## [1] "Age"
```
Labels can be added to new variables or changed via 

```r
label(labelled$bmd$age) <- "Age (years)"
label(labelled$bmd$age)
```

```
## [1] "Age (years)"
```
Where units have been defined in the SecuTrial database, they can be accessed or changed analogously (here, age had no unit assigned, but we can add one).

```r
units(labelled$bmd$age)
```

```
## NULL
```

```r
units(labelled$bmd$age) <- "years"
units(labelled$bmd$age)
```

```
## [1] "years"
```
There is a drawback to the attribute based approach - labels will not be propagated if variables are derived and may be lost if variables are edited.

Currently, `label_secuTrial` should be used prior to `dates_secuTrial` or `factorize_secuTrial` so that labels and units are propagated to factor and date variables.


 
#### Prepare factors
It is often useful to have categorical variables as factors (R knows how to handle factors). secuTrialR can prepare factors easily.

```r
factors <- factorize_secuTrial(ctu05_raw)
```
This functions loops through each table of the dataset, creating new factor variables where necessary. The new variables are the same as the original but with `.factor` appended (i.e. a new variable called `sex.factor` would be added to the relevant form).


```r
# original variable
str(factors$ctu05baseline$gender)
```

```
##  int [1:17] 1 NA NA 2 1 2 1 NA NA 1 ...
```

```r
# factor
str(factors$ctu05baseline$gender.factor)
```

```
##  Factor w/ 2 levels "male","female": 1 NA NA 2 1 2 1 NA NA 1 ...
```

```r
# cross tabulation
table(original = factors$ctu05baseline$gender, factor = factors$ctu05baseline$gender.factor)
```

```
##         factor
## original male female
##        1    5      0
##        2    0      5
```


#### Prepare dates
Date(time)s are a very common data type. They cannot be easily used though in their export format. This is also easily rectified in secuTrialR:



```r
dates <- dates_secuTrial(ctu05_raw)
```

Date variables are converted to `Date` class, and datetimes are converted to `POSIXct` class. Rather than overwriting the original variable, new variables are added with the new class. This is a safetly mechanism in case `NA`s are accidentally created.


```r
dates$ctu05baseline[c(1,7), c("aspirin_start", "aspirin_start.date", "hiv_date", "hiv_date.datetime")]
```

```
##   aspirin_start aspirin_start.date     hiv_date   hiv_date.datetime
## 1            NA               <NA> 201903052356 2019-03-05 23:56:00
## 7      20060301         2006-03-01           NA                <NA>
```


#### Recommended approach if not using `read_secuTrial`


```r
f <- "PATH_TO_FILE"
d <- read_secuTrial_export(f)
l <- label_secuTrial(d)
fa <- factorize_secuTrial(l)
dat <- dates_secuTrial(fa)

# or, if you like pipes
library(magrittr)
f <- "PATH_TO_FILE"
d <- read_secuTrial_export(f)
dat <- d %>% 
  label_secuTrial() %>%
  factorize_secuTrial() %>%
  dates_secuTrial()
```

</details>

### Exploratory helpers
`secuTrialR` has a couple of functions to help get to grips with a secuTrial data export. They are intended to be used in an exploratory manner only.

#### Form status summary statistics
If you are not sure about how complete the data in you export is, it may be useful to get a quick overview of how well the forms
have been filled.


```r
count_summary <- form_status_summary(ctu05)
tail(count_summary)
```

```
##             form_name partly_filled completely_filled empty with_warnings
## 5        ctu05allmedi             1                16     0             0
## 6       ctu05baseline             3                14     0             0
## 7        ctu05outcome             1                12     0             0
## 8            ctu05sae             0                 2     0             0
## 9  ctu05studyterminat             0                10     0             0
## 10     ctu05treatment             0                11     0             0
##    with_errors partly_filled.percent completely_filled.percent
## 5            0            0.05882353                 0.9411765
## 6            0            0.17647059                 0.8235294
## 7            0            0.07692308                 0.9230769
## 8            0            0.00000000                 1.0000000
## 9            0            0.00000000                 1.0000000
## 10           0            0.00000000                 1.0000000
##    empty.percent with_warnings.percent with_errors.percent form_count
## 5              0                     0                   0         17
## 6              0                     0                   0         17
## 7              0                     0                   0         13
## 8              0                     0                   0          2
## 9              0                     0                   0         10
## 10             0                     0                   0         11
```

As you can see, the majority of forms has been completeley filled. None of the forms were saved empty, with warnings or with errors.
For a more patient id centered statistic you can perform the following.


```r
form_status_counts(ctu05)
```

This will give you a count based overview per patient id and form. Please note that both `form_status_summary` 
and `form_status_counts` only work with saved forms since unsaved form data is not available in secuTrial exports.

#### Visit plan
secuTrialR can provide a depiction of the visit structure, although only where the visit plan is fixed:

```r
vs <- visit_structure(ctu05)
plot(vs)
```
<!-- PLOT METHOD DIRECTLY FOR secuTrialdata objects? -->


#### Linking different forms

Linkages amongst forms can be explored with the `links_secuTrial` function. This relies on the `igraph` package to create a network. It is possible to interact with the network, e.g. move nodes around in order to read the labels better. The device ID is returned to the console, but can be ignored. Forms are plotted in deep yellow, variables in light blue.


```r
links_secuTrial(bmd_export)
```
![](inst/extdata/map.png)
<!-- Figure has to be generated outside of the Rmd file - resize the window and select view/"fit to screen", export it to a PDF and then convert it to a PNG -->


## For contributors
### Testing with devtools


```r
# run tests
devtools::test("secuTrialR")
# spell check -> will contain some technical terms beyond the below list which is fine
ignore_words <- c("AdminTool", "allforms", "casenodes", "CDMS", "codebook",
                  "codebooks", "datetime" ,"dir" ,"Hmisc" ,"igraph",
                  "labelled", "mnp", "savedforms", "secutrial", "secuTrial", 
                  "secuTrialdata", "tcltk", "tibble")
devtools::spell_check("secuTrialR", ignore = ignore_words)
```

### Linting with lintr


```r
# lint the package -> should be clean
library(lintr)
lint_package("secuTrialR", linters = with_defaults(camel_case_linter = NULL,
                                                   object_usage_linter = NULL,
                                                   line_length_linter(125)))
```

### Generating the README file

The README file contains both standard text and interpreted R code. It must therefore be compiled. Changes should be made in the `README.Rmd` file and the file "knited" with R. This is easiest with RStudio, but other methods are available.


```r
library("knitr")
knit("README.Rmd")
```

### Guidelines for contributors

In order to contribute to this R package you should fork the main repository.
After you have made your changes please run the 
[tests](README.md#testing-with-devtools)
and 
[lint](README.md#linting-with-lintr) your code as 
indicated above. If all tests pass and linting confirms that your 
coding style conforms you can send a pull request (PR).  
The PR should have a description to help the reviewer understand what has been 
added/changed. New functionalities must be thoroughly documented, have examples 
and should be accompanied by at least one [test](tests/testthat/) to ensure long term 
robustness. The PR will only be reviewed if all travis checks are successful. 
The person sending the PR should not be the one merging it.

A depiction of the core functionalities for loading can be found [here](inst/extdata/secuTrialR.png).
