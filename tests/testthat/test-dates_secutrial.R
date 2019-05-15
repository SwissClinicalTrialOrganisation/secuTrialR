context("dates")


# check individual variables
dat <- load_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_shortnames.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_shortnames (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_shortnames (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (shortnames)",
          expect_equal(class(f$outcome$death_date.date), "Date"))

dat <- load_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_longnames.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_longnames (warn)",
          expect_warning(f <- dates_secuTrial(dat, warn = TRUE)))
test_that("loading data: CTU05_longnames (no warn)",
          expect_warning(f <- dates_secuTrial(dat), regexp = NA))

f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (longnames)", expect_equal(class(f$ctu05outcome$death_date.date), "Date"))

dat <- load_secuTrial_export(system.file("extdata",
                                         "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_longnames_sep_ref",
          expect_warning(f <- dates_secuTrial(dat)))
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

n <- sum(unlist(lapply(f, function(x) sum(sapply(x, function(y) class(y) == "Date")))))
test_that("number of variables", expect_equal(n, 12))

# test for any .date at end of names
test_that("dates detected", expect_true(any(grep("\\.date$", names(f$ctu05baseline)))))

# warnings for trying to run dates again
# d <- suppressWarnings(dates_secuTrial(f))
# w <- warnings()
# n <- names(w)
test_that("second dates warning",
          expect_warning(dates_secuTrial(f), "dates already added"))

# test dates methods

d <- c("2010-10-15", "2019-05-15")
x <- secuTrialR:::dates_secuTrial(as.factor(d))
test_that("factor dates",
          expect_equal(x, as.Date(d)))
x <- secuTrialR:::dates_secuTrial(as.numeric(gsub("-", "", d)), format = "%Y%m%d")
test_that("numeric dates",
          expect_equal(x, as.Date(d)))
x <- secuTrialR:::dates_secuTrial(d)
test_that("character dates",
          expect_equal(x, as.Date(d)))
d <- c(NA, NA)
x <- secuTrialR:::dates_secuTrial(d)
test_that("logical dates",
          expect_equal(x, as.Date(d)))



