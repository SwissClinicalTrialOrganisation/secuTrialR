context("durations")

# cannot currently test properly, only numeric method


v <- c(1, # 1 min
       10, # 10 min
       100, # 1 hour
       1000) # 10 hours
x <- durations_secuTrial(v, 4, "h-m")
test_that("h-m", expect_equal(x, c(1, 10, 60, 600)))

v <- c(v,
       10000, # 1 day
       100000, # 10 days
       1000000, # 1 month
       10000000, # 10 months
       100000000) # 1 year
x <- durations_secuTrial(v, 10, "y-m-d-h-m")
x2 <- durations_secuTrial(v, 10, "y-m-d-h-m", 365.25)
test_that("h-m", {
  expect_equal(x, c(1, 10, 60, 600, # 10 hours
                    24*60, # 1 day
                    24*60*10, # 10 days
                    24*60*(365/12), # 1 month
                    24*60*(365/12)*10, # 10 months
                    24*60*365)) # 1 year

  expect_equal(x2, c(1, 10, 60, 600, # 10 hours
                     24*60, # 1 day
                     24*60*10, # 10 days
                     24*60*(365.25/12), # 1 month
                     24*60*(365.25/12)*10, # 10 months
                     24*60*365.25)) # 1 year

})


v <- c(1, 1010, 100, 1000, 2236)
x <- secuTrialR::times_secuTrial(v, 4, "hh:mm")
test_that("h-m", expect_equal(x, c("00:01", "10:10", "01:00", "10:00", "22:36")))
