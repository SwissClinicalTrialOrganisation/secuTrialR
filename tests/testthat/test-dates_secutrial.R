context("dates")

skip_on_cran()

# check individual variables
dat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "snames",
                                      "s_export_CSV-xls_CTU05_short_miss_en_utf8.zip",
                                      package = "secuTrialR"))
test_that("loading data: CTU05_shortnames (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_shortnames (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (shortnames)",
          expect_equal(class(f$outcome$death_date.date), "Date"))

test_that("metadata variable is date (shortnames)",
          expect_equal(class(f$cn$mnpvisstartdate.date), "Date"))
test_that("metadata variable is date",
          expect_equal(class(f$atallmedi$mnpvispdt.date), "Date"))
test_that("metadata variable is date",
          expect_equal(class(f$atae1$newdate.date), "Date"))
test_that("metadata variable is date",
          expect_equal(class(f$atcvp$newmnpvispdt.date), "Date"))
test_that("metadata variable is date",
          expect_equal(class(f$atcn$newvisitstartdate.date), "Date"))
test_that("metadata variable is date",
          expect_equal(class(f$atae1$olddate.date), "Date"))
test_that("metadata variable is date",
          expect_equal(class(f$atcvp$oldmnpvispdt.date), "Date"))
test_that("metadata variable is date",
          expect_equal(class(f$atcn$oldvisitstartdate.date), "Date"))
test_that("metadata variable is datetime",
          expect_equal(class(f$atcn$changedate.datetime), c("POSIXct", "POSIXt")))
test_that("metadata variable is datetime",
          expect_equal(class(f$atmiv$editdate.datetime), c("POSIXct", "POSIXt")))
test_that("metadata variable is datetime",
          expect_equal(class(f$atmiv$editdate.datetime), c("POSIXct", "POSIXt")))
test_that("metadata variable is datetime",
          expect_equal(class(f$qac$qacdate.datetime), c("POSIXct", "POSIXt")))
test_that("metadata variable is datetime",
          expect_equal(class(f$atsae$mnpaefudt.datetime), c("POSIXct", "POSIXt")))
test_that("metadata variable is datetime",
          expect_equal(class(f$atsae$mnplastedit.datetime), c("POSIXct", "POSIXt")))
test_that("metadata variable is datetime",
          expect_equal(class(f$atallmedi$mnpvisfdt.datetime), c("POSIXct", "POSIXt")))
test_that("number of datetime metadata variables",
          expect_equal(sum(sapply(sapply(f$treatment, class), function(x) identical(x, c("POSIXct", "POSIXt")))), 2))
test_that("number of date metadata variables",
          expect_equal(sum(sapply(sapply(f$atae1, class), function(x) identical(x, "Date"))), 2))


dat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
                                      "s_export_CSV-xls_CTU05_long_miss_en_utf8.zip",
                                      package = "secuTrialR"))
test_that("loading data: CTU05_longnames (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_longnames (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))

f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (longnames)", expect_equal(class(f$ctu05outcome$death_date.date), "Date"))

test_that("number of datetime metadata variables",
          expect_equal(sum(sapply(sapply(f$ctu05treatment, class), function(x) identical(x, c("POSIXct", "POSIXt")))), 2))
test_that("number of date metadata variables",
          expect_equal(sum(sapply(sapply(f$atadverseevents, class), function(x) identical(x, "Date"))), 2))

dat <- read_secuTrial_raw(system.file("extdata", "sT_exports", "lnames",
                                      "s_export_CSV-xls_CTU05_long_ref_miss_en_utf8.zip",
                                      package = "secuTrialR"))
test_that("loading data: CTU05_long_sep_ref (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_long_sep_ref (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (long_sep_ref)",
          expect_equal(class(f$ctu05outcome$death_date.date), "Date"))

f <- suppressWarnings(dates_secuTrial(dat))

# nolint start
# check that the correct number of variables are there
# c("Checked Date (dd.mm.yyyy hh:mm)", "Checked Date (dd.mm.yyyy)", "Checked Date (yyyy)", "Checked Time (hh:mm)", "Date (dd.mm.yyyy)")

# l <- dat$items[dat$items$itemtype %in% c("Checked Date (dd.mm.yyyy)", "Date (dd.mm.yyyy)"), ]
# lf <- merge(l, dat$questions, by = "fgid")
#
# n <- unlist(lapply(f, function(x) names(x)[sapply(x, function(y) class(y) == "Date")]))
# names(n) <- NULL
# gsub("\\.date$", "", n) %in% as.character(l$ffcolname)
# as.character(l$ffcolname) %in% gsub("\\.date$", "", n)
# nolint end

meta_datevars <- c("mnpvispdt", "mnpvisstartdate", "newdate", "newmnpvispdt",
                   "newvisitstartdate", "olddate", "oldmnpvispdt", "oldvisitstartdate")
meta_datetimevars <- c("changedate", "editdate", "mnpaedate", "mnpaefudt", "mnpcrtdt",
                       "mnplastedit", "mnpvisfdt", "qacdate", "sdvdate", "uploaddate", "versiondate")


n <- sum(unlist(lapply(f, function(x) sum(sapply(x, function(y) class(y)[1] == "Date")))))
n_should <- sum(unlist(lapply(f, function(x) sum(names(x) %in% meta_datevars)))) + 12
test_that("number of date variables", expect_equal(n, n_should))

n <- sum(unlist(lapply(f, function(x) sum(sapply(x, function(y) class(y)[1] == "POSIXct")))))
n_should <- sum(unlist(lapply(f, function(x) sum(names(x) %in% meta_datetimevars)))) + 1
test_that("number of date variables", expect_equal(n, n_should))

# test for any .date at end of names
test_that("dates detected",
          expect_true(any(grep("\\.date$", names(f$ctu05baseline)))))
test_that("datetimes detected",
          expect_true(any(grep("\\.datetime$", names(f$ctu05baseline)))))

test_that("datetime correctly parsed",
          expect_equal(as.numeric(format(f$ctu05baseline$hiv_date.datetime,
                                         "%Y%m%d%H%M")),
                       f$ctu05baseline$hiv_date)
)

# warnings for trying to run dates again
# nolint start
# d <- suppressWarnings(dates_secuTrial(f))
# w <- warnings()
# n <- names(w)
# nolint end
test_that("second dates warning",
          expect_warning(dates_secuTrial(f), "dates already added"))

# test methods

d <- c("2010-10-15", "2019-05-15")
label(d) <- "foo"
units(d) <- "bar"
fd <- as.factor(d)
label(fd) <- "foo"
units(fd) <- "bar"
nd <- as.numeric(gsub("-| |:", "", d))
label(nd) <- "foo"
units(nd) <- "bar"

x <- secuTrialR:::dates_secuTrial(fd)
test_that("factor dates", {
  expect_equal(as.numeric(x), as.numeric(as.Date(d)))
  expect_equal(label(x), "foo")
  expect_equal(units(x), "bar")
})
x <- secuTrialR:::dates_secuTrial(nd, format = "%Y%m%d")
test_that("numeric dates", {
  expect_equal(as.numeric(x), as.numeric(as.Date(d)))
  expect_equal(label(x), "foo")
  expect_equal(units(x), "bar")
})
x <- secuTrialR:::dates_secuTrial(d)
test_that("character dates", {
  expect_equal(as.numeric(x), as.numeric(as.Date(d)))
  expect_equal(label(x), "foo")
  expect_equal(units(x), "bar")
})
d <- c(NA, NA)
label(d) <- "foo"
units(d) <- "bar"
x <- secuTrialR:::dates_secuTrial(d)
test_that("logical dates", {
  expect_equal(as.numeric(x), as.numeric(as.Date(d)))
  expect_equal(label(x), "foo")
  expect_equal(units(x), "bar")
})



d <- c("2010-10-15 12:15", "2019-05-15 12:15")
label(d) <- "foo"
units(d) <- "bar"
fd <- as.factor(d)
label(fd) <- "foo"
units(fd) <- "bar"
nd <- as.numeric(gsub("-| |:", "", d))
label(nd) <- "foo"
units(nd) <- "bar"

x <- secuTrialR:::datetimes_secuTrial(fd)
test_that("factor datetimes", {
  expect_equal(as.numeric(x), as.numeric(as.POSIXct(d)))
  expect_equal(label(x), "foo")
  expect_equal(units(x), "bar")
})

x <- secuTrialR:::datetimes_secuTrial(nd,
                                      format = "%Y%m%d%H%M")
test_that("numeric datetimes", {
  expect_equal(as.numeric(x), as.numeric(as.POSIXct(d)))
  expect_equal(label(x), "foo")
  expect_equal(units(x), "bar")
})

x <- secuTrialR:::datetimes_secuTrial(d)
test_that("character datetimes", {
  expect_equal(as.numeric(x), as.numeric(as.POSIXct(d)))
  expect_equal(label(x), "foo")
  expect_equal(units(x), "bar")
})
d <- c(NA, NA)
label(d) <- "foo"
units(d) <- "bar"
x <- secuTrialR:::datetimes_secuTrial(d)
test_that("logical datetimes", {
  expect_equal(as.numeric(x), as.numeric(as.POSIXct(d)))
})

d <- dates_secuTrial(c("2010-10-15", "2019-05-15"))
e <- secuTrialR:::datetimes_secuTrial(c("2010-10-15 12:15", "2019-05-15 12:15"))
test_that("redating creates an error", {
  expect_warning(dates_secuTrial(d))
  expect_warning(secuTrialR:::datetimes_secuTrial(e))
})

# test warnings for incomplete conversion
path <- system.file("extdata", "sT_exports", "encodings",
                    "s_export_CSV-xls_TES05_short_ref_en_iso8859-15.zip",
                    package = "secuTrialR")

tes05_raw <- read_secuTrial_raw(path)

test_that("loading data: TES05 incomplete dates (warn)", {
  expect_warning(f <- dates_secuTrial(tes05_raw, warn = TRUE))
})

# TODO: include tests with a dataset that was exported with duplicate meta data into all tables
#       include tests with dates and datetimes in repetitions
