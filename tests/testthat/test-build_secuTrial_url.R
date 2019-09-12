context("build url")

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
  expect_equal(build_secuTrial_url(server2, instance, customer, docid = docid), sT_url_customer)
})

# warning when parameter list incomplete
warn_docid <- "A valid link to a secuTrial form always requires server, instance, customer, projid and docid parameters."
warn_projid <- "A valid link to a secuTrial project page always requires server, instance, customer and projid parameters."
warn_customer <- "A valid link to a secuTrial customer page always requires server, instance and customer parameters."

test_that("Warn incomplete parameters", {
  expect_warning(build_secuTrial_url(server, instance = instance, projid = projid, docid = docid), warn_docid)
  expect_warning(build_secuTrial_url(server, instance = instance, customer = customer, docid = docid), warn_docid)
  expect_warning(build_secuTrial_url(server, customer = customer, projid = projid, docid = docid), warn_docid)
  expect_warning(build_secuTrial_url(server, instance = instance, projid = projid), warn_projid)
  expect_warning(build_secuTrial_url(server, customer = customer, projid = projid), warn_projid)
  expect_warning(build_secuTrial_url(server, customer = customer), warn_customer)
})
