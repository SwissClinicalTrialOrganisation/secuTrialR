context("dates")


# check individual variables
dat <- read_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_shortnames.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_shortnames (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_shortnames (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (shortnames)",
          expect_equal(class(f$outcome$death_date.date), "Date"))

dat <- read_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_longnames.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_longnames (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_longnames (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))

f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (longnames)", expect_equal(class(f$ctu05outcome$death_date.date), "Date"))

dat <- read_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_longnames_sep_ref (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_longnames_sep_ref (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (longnames_sep_ref)",
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

n <- sum(unlist(lapply(f, function(x) sum(sapply(x, function(y) class(y)[1] == "Date")))))
test_that("number of date variables", expect_equal(n, 12))
n <- sum(unlist(lapply(f, function(x) sum(sapply(x, function(y) class(y)[1] == "POSIXct")))))
test_that("number of date variables", expect_equal(n, 1))

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

# TODO: include tests with a dataset that was exported with duplicate meta data into all tables
#       include tests with dates and datetimes in repetitions
