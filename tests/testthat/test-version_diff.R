context("version increment")

return_code <- system("git diff | grep 'Version:'", intern = TRUE)

# should return length 2 if the Version number has changed
test_that("Version number has been incremented", {
  expect_true(length(return_code) > 2)
})
