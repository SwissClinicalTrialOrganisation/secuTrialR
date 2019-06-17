context("durations")

# cannot currently test properly, only numeric method


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
x2 <- durations_secuTrial(v, "y-m-d-h-m", 365.25)
test_that("y-m-d-h-m", {
  expect_equal(x, c(NA, 1, 10, 60, 600, # 10 hours
                    24 * 60, # 1 day
                    24 * 60 * 10, # 10 days
                    24 * 60 * (365 / 12), # 1 month
                    24 * 60 * (365 / 12) * 10, # 10 months
                    24 * 60 * 365)) # 1 year

  expect_equal(x2, c(NA, 1, 10, 60, 600, # 10 hours
                     24 * 60, # 1 day
                     24 * 60 * 10, # 10 days
                     24 * 60 * (365.25 / 12), # 1 month
                     24 * 60 * (365.25 / 12) * 10, # 10 months
                     24 * 60 * 365.25)) # 1 year

})


v <- c(NA, 1, 1010, 100, 1000, 2236)
x <- secuTrialR:::times_secuTrial(v, "hh:mm")
test_that("hh:mm", expect_equal(x, c(NA, "00:01", "10:10", "01:00", "10:00", "22:36")))



sdat <- read_secuTrial(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_shortnames.zip",
                                   package = "secuTrialR"))
ldat <- read_secuTrial(system.file("extdata",
                                   "s_export_CSV-xls_CTU05_longnames.zip",
                                   package = "secuTrialR"))

sdur <- durations_secuTrial(sdat)
ldur <- durations_secuTrial(ldat)
ldur$ctu05baseline$age
ldur$ctu05baseline$age.dur

test_that("ldat, sdat", {
  expect_equal(as.numeric(ldur$ctu05baseline$age),
               as.numeric(ldur$ctu05baseline$age.dur))

  expect_equal(as.numeric(sdur$ctu05baseline$age),
               as.numeric(sdur$ctu05baseline$age.dur))

  expect_equal(as.character(ldur$ctu05sae$sae_end_time.time), c("12:07", "18:06"))
  expect_equal(label(ldur$ctu05sae$sae_end_time.time), "Timepoints (clock time)")
  expect_equal(as.character(sdur$ctu05sae$sae_end_time.time), c("12:07", "18:06"))
  expect_equal(label(sdur$ctu05sae$sae_end_time.time), "Timepoints (clock time)")
})
