context("build url")

skip_on_cran()

# inputs for the build_secuTrial_url() function
server <- "server.secutrial.com"
server2 <- "https://server.secutrial.com/"
instance <- "INSTANCE-01"
customer <- "CUSTOMER-01"
projid <- "1234"
docid <- "5678"

# expected build_secuTrial_url() outputs
sT_url_full <- paste0("https://server.secutrial.com/apps/WebObjects/INSTANCE-01.woa/wa/",
                      "choose?customer=CUSTOMER-01&projectid=1234&docid=5678")
sT_url_customer <- "https://server.secutrial.com/apps/WebObjects/INSTANCE-01.woa/wa/choose?customer=CUSTOMER-01"

test_that("All urls built correctly", {
  expect_equal(build_secuTrial_url(server, instance, customer, projid, docid), sT_url_full)
  expect_equal(build_secuTrial_url(server2, instance, customer, projid, docid), sT_url_full)
  expect_equal(suppressWarnings(build_secuTrial_url(server2, instance, customer, docid = docid)), sT_url_customer)
})

# warning when parameter list incomplete
warn_docid <- "'projid', 'customer' and 'instance' must all be provided with 'docid'"
warn_projid <- "'customer' and 'instance' must all be provided with 'projid'"
warn_customer <- "'instance' must be provided with 'customer'"

test_that("Warn incomplete parameters", {
  expect_warning(build_secuTrial_url(server, instance = instance, projid = projid, docid = docid), warn_docid)
  expect_warning(build_secuTrial_url(server, instance = instance, customer = customer, docid = docid), warn_docid)
  expect_warning(build_secuTrial_url(server, customer = customer, projid = projid, docid = docid), warn_docid)
  expect_warning(build_secuTrial_url(server, instance = instance, projid = projid), warn_projid)
  expect_warning(build_secuTrial_url(server, customer = customer, projid = projid), warn_projid)
  expect_warning(build_secuTrial_url(server, customer = customer), warn_customer)
})
