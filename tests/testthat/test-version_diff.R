context("version increment - DESCRIPTION/README badge")

skip_on_cran()
skip_on_appveyor()
skip_on_travis()

return_code <- system("git diff", intern = TRUE)

# only if there are diffs this check should be made
# i.e. users of the package should not be confronted
# with this test
if (length(return_code) == 0) {
  skip("No diffs in git.")
}

grep_version <- grep("Version: \\d\\.\\d\\.\\d", return_code)
grep_badge <- grep("https://img.shields.io/badge/", return_code)

# should return length 2 if the Version number has changed
# this will only check if the developer locally runs the tests
# travis will skip this
test_that("Version number has been incremented in DESCRIPTION", {
  expect_true(length(grep_version) >= 2)
})

test_that("Version number has been incremented in README badge", {
  expect_true(length(grep_badge) >= 2)
})
