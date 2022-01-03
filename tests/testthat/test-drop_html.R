test_that("an HTML taglist is being returned", {
  expect_equal(class(drop_html(title = "A", journal = "B",
                         authors = "C", year = "D",
                         cite_key = "E", url = "F", include_qr = "embed", style = "modern")),
               class(htmltools::tagList()))
})
