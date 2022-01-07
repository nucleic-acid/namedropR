test_that("an HTML taglist is being returned", {
  expect_equal(class(drop_html(title = "A", journal = "B",
                         authors = "C", year = "D",
                         cite_key = "E", url = "F", include_qr = "embed",
                         output_dir = "visual_citations", style = "modern",
                         use_xaringan = FALSE)),
               class(htmltools::tagList()))
})

test_that("inputs are correct", {
  expect_error(drop_html(title = 1, journal = "B",
            authors = "C", year = "D",
            cite_key = "E", url = "F", include_qr = "embed",
            output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

  expect_error(drop_html(title = "A", journal = 2,
            authors = "C", year = "D",
            cite_key = "E", url = "F", include_qr = "embed",
            output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

  expect_error(drop_html(title = "A", journal = "B",
            authors = 3, year = "D",
            cite_key = "E", url = "F", include_qr = "embed",
            output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

  expect_error(drop_html(title = "A", journal = "B",
            authors = "C", year = FALSE,
            cite_key = "E", url = "F", include_qr = "embed",
            output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

  #cite_key wrong format
  expect_error(drop_html(title = "A", journal = "B",
            authors = "C", year = "D",
            cite_key = FALSE, url = "F", include_qr = "embed",
            output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

  #cite key empty
  expect_error(drop_html(title = "A", journal = "B",
                         authors = "C", year = "D",
                         cite_key = "", url = "F", include_qr = "embed",
                         output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

  expect_error(drop_html(title = "A", journal = "B",
            authors = "C", year = "D",
            cite_key = "E", url = FALSE, include_qr = "embed",
            output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

  expect_error(drop_html(title = "A", journal = "B",
            authors = "C", year = "D",
            cite_key = "E", url = "F", include_qr = 123,
            output_dir = "visual_citations", style = "modern", use_xaringan = FALSE))

})

