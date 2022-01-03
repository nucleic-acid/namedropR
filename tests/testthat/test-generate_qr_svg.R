cite_key <- "test_string"
url <- "https://en.wikipedia.org/"

test_that("QR code was created and stored to tempdir", {
  generate_qr_svg(url = url, cite_key = cite_key)
  filename = paste0(tempdir(),"/qr_codes/qr_",cite_key, ".svg")
  expect_true(file.exists(filename))
})
