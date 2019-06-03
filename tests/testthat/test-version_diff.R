context("version increment")

return_code <- system("git diff", intern = TRUE)
grep_version <- grep("Version: \\d\\.\\d\\.\\d", return_code)

# should return length 2 if the Version number has changed
test_that("Version number has been incremented", {
  expect_true(length(grep_version) >= 2)
})
