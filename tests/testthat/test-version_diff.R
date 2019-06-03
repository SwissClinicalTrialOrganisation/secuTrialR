context("version increment")

return_code <- system("git diff ../../DESCRIPTION | grep 'Version:'", intern = TRUE)

# should return length 2 if the Version number has changed
test_that("Version number has been incremented", {
  expect_equal(length(return_code), 2)
})
