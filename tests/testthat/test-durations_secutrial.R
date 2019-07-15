context("durations")

secuTrialR:::format2length("y-m-d")
secuTrialR:::format2length("y-m-d", 364)

v <- c(NA,
       1, # 1 min
       10, # 10 min
       100, # 1 hour
       1000) # 10 hours
x <- durations_secuTrial(v, "h-m")
test_that("h-m", expect_equal(x, c(NA, 1, 10, 60, 600)))

v <- c(v,
       10000, # 1 day
       100000, # 10 days
       1000000, # 1 month
       10000000, # 10 months
       100000000) # 1 year
x <- durations_secuTrial(v, "y-m-d-h-m")
x2 <- durations_secuTrial(v, "y-m-d-h-m", days_in_year = 365)
test_that("y-m-d-h-m", {
  expect_equal(x, c(NA, 1, 10, 60, 600, # 10 hours
                    24 * 60, # 1 day
                    24 * 60 * 10, # 10 days
                    24 * 60 * (365.25 / 12), # 1 month
                    24 * 60 * (365.25 / 12) * 10, # 10 months
                    24 * 60 * 365.25)) # 1 year

  expect_equal(x2, c(NA, 1, 10, 60, 600, # 10 hours
                     24 * 60, # 1 day
                     24 * 60 * 10, # 10 days
                     24 * 60 * (365 / 12), # 1 month
                     24 * 60 * (365 / 12) * 10, # 10 months
                     24 * 60 * 365)) # 1 year

})


v <- c(NA, 1, 1010, 100, 1000, 2236)
x <- secuTrialR:::times_secuTrial(v, "hh:mm")
z <- secuTrialR:::times_secuTrial(v, "mm:ss")
test_that("hh:mm", {
  expect_equal(x, c(NA, "00:01", "10:10", "01:00", "10:00", "22:36"))
  expect_equal(z, c(NA, "00:01", "10:10", "01:00", "10:00", "22:36"))
})



sdat <- read_secuTrial(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_shortnames.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_longnames.zip",
                                   package = "secuTrialR"))
s <- read_secuTrial_export(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_shortnames.zip",
                                   package = "secuTrialR"))
l <- read_secuTrial_export(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_longnames.zip",
                                   package = "secuTrialR"))


test_that("ldat, sdat", {

  expect_warning(sdur <- durations_secuTrial(sdat))
  expect_warning(ldur <- durations_secuTrial(ldat))

  expect_warning(durations_secuTrial(s), regexp = NA)
  expect_warning(durations_secuTrial(l), regexp = NA)

  expect_equal(as.character(ldur$ctu05sae$sae_end_time.time), c("12:07", "18:06"))
  expect_equal(label(ldur$ctu05sae$sae_end_time.time), "Timepoints (clock time)")
  expect_equal(as.character(sdur$sae$sae_end_time.time), c("12:07", "18:06"))
  expect_equal(label(sdur$sae$sae_end_time.time), "Timepoints (clock time)")

})


data_dir <- system.file("extdata", "s_export_CSV-xls_TES05.zip",
                        package = "secuTrialR")
dat <- read_secuTrial(data_dir)
x <- dat$intervals
x2 <- dat$bl
fup <- dat$fuvisit
fup$vis <- ifelse(grepl("One", fup$visit_name), 1,
                  ifelse(grepl("Two", fup$visit_name), 2,
                         ifelse(grepl("Three", fup$visit_name), 3, 4)))
fup <- fup[, !grepl("mnp|centre|visit|sigs", names(fup))]
fupr <- reshape(fup, direction = "wide",
                idvar = c("pat_id"),
                v.names = names(fup)[!names(fup) %in% c("pat_id", "vis")],
                timevar = "vis")
fupr$dint <- as.numeric(difftime(fupr$v_date_ddhhyyyy.date.4,
                                 fupr$v_date_ddhhyyyy.date.1, units = "d"))
blfup <- merge(dat$bl, fupr, by = "pat_id")
blfup <- blfup[, !grepl("mnp|centre|visit|sigs", names(blfup))]


blfup2 <- within(blfup, {
  yint <- as.numeric(v_date_ddhhyyyy.date.4 - bl_date_ddhhyyyy.date) / 365
  ymdhmint <- as.numeric(difftime(v_date_ddhhyyyyhhmm.datetime.3, bl_date_ddhhyyyyhhmm.datetime, units = "mins"))
  hmsint <- as.numeric(difftime(v_date_ddhhyyyyhhmm.datetime.3, bl_date_ddhhyyyyhhmm.datetime, units = "sec"))
})

# dat$is[grepl("Interval", dat$is$itemtype), ]


test_that("results as expected", {
  expect_equal(x$ymint.dur[1:3], c(0, 10*12, 4*12+10))
  expect_equal(x$msint.dur[1:3], c(45*60+2, 0, 14*60+47))
  expect_equal(x$ymdint.dur[1:3], c(15, 30*365.25, 17))
  expect_equal(x$ymint.dur[1:3], c(0, 120, 58))

  dint <- as.numeric(fupr$dint[match(x$pat_id, fupr$pat_id)])
  expect_equal(as.numeric(x$dint), dint)

  # y <- blfup2[match(x$pat_id, blfup2$pat_id), ]
  # expect_equal(as.numeric(x$yint), y$yint)
  # part rounding, partly secutrial seems to calculate wrongly for [3]

  expect_equal(x2$bl_time_hhmmss.time[1], c("10:52:43"))
  expect_equal(x2$bl_time_hhmm.time[1], c("10:52"))
})
