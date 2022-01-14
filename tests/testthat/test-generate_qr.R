test_that("QR code is created and is returned as matrix", {
  expect_equal(
    class(generate_qr("https://en.wikipedia.org/")),
    class(qrcode::qr_code("https://en.wikipedia.org/"))
  )
})
