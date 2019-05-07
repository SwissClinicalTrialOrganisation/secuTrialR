context("dates")


# check individual variables
dat <- load_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_shortnames.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_shortnames", expect_warning(f <- dates_secuTrial(dat)))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (shortnames)", expect_equal(class(f$outcome$death_date.date), "Date"))

dat <- load_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_longnames.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_longnames", expect_warning(f <- dates_secuTrial(dat)))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (longnames)", expect_equal(class(f$ctu05outcome$death_date.date), "Date"))

dat <- load_secuTrial_export(system.file("extdata", "s_export_CSV-xls_CTU05_longnames_sep_ref.zip",
                                         package = "secuTrialR"))
test_that("loading data: CTU05_longnames_sep_ref", expect_warning(f <- dates_secuTrial(dat)))
f <- suppressWarnings(dates_secuTrial(dat))
test_that("outcome variable is date (longnames_sep_ref)", expect_equal(class(f$ctu05outcome$death_date.date), "Date"))

f <- suppressWarnings(dates_secuTrial(dat))

# check that the correct number of variables are there
# c("Checked Date (dd.mm.yyyy hh:mm)", "Checked Date (dd.mm.yyyy)", "Checked Date (yyyy)", "Checked Time (hh:mm)", "Date (dd.mm.yyyy)")

# l <- dat$items[dat$items$itemtype %in% c("Checked Date (dd.mm.yyyy)", "Date (dd.mm.yyyy)"), ]
# lf <- merge(l, dat$questions, by = "fgid")
#
# n <- unlist(lapply(f, function(x) names(x)[sapply(x, function(y) class(y) == "Date")]))
# names(n) <- NULL
# gsub("\\.date$", "", n) %in% as.character(l$ffcolname)
# as.character(l$ffcolname) %in% gsub("\\.date$", "", n)

n <- sum(unlist(lapply(f, function(x) sum(sapply(x, function(y) class(y) == "Date")))))
test_that("number of variables", expect_equal(n, 12))

# test for any .date at end of names
test_that("dates detected", expect_true(any(grep("\\.date$", names(f$ctu05baseline)))))

