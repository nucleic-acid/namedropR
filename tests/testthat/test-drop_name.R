bib_path <- system.file("testdata", "sample.bib", package="namedropR")
test_bibfile <- RefManageR::ReadBib(bib_path)
empty_path <- system.file("testdata", "empty.bib", package="namedropR")
empty_bibfile <- RefManageR::ReadBib(empty_path)

test_that("Bibliographies are correctly read", {
  expect_message(drop_name(bib = bib_path, cite_key = "Eschrich1983", inline = TRUE), "Bibliography file successfully read")
  expect_error(drop_name(bib = "wrong_path", cite_key = "Eschrich1983", inline = TRUE), "file not found")
})

test_that("inline = TRUE returns the correct objects", {
  expect_true(any(class(drop_name(bib = test_bibfile, cite_key = "collaboration_2019_ApJL", export_as = "html", inline = TRUE)) == "shiny.tag.list"))
})

test_that("inline = FALSE returns the correct file path as character", {
  expect_type(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", inline = FALSE), "character")
  expect_true(file.exists(here::here(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", inline = FALSE))))
  unlink(here::here("visual_citations"), recursive = TRUE)
})

test_that("reading and extracting bibtex information is error tolerant", {
  expect_error(drop_name(bib = "wrong_file_path", cite_key = "Eschrich1983"), "file not found")
  expect_error(drop_name(bib = test_bibfile, cite_key = "wrong_cite_key"), "entry not found")
  expect_error(drop_name(bib = empty_bibfile, cite_key = "Eschrich1983", "is empty"))
})

test_that("input argument types are correct", {
  expect_error(drop_name(bib = 123, , inline = TRUE))
  expect_error(drop_name(bib = test_bibfile, cite_key = 1234, inline = TRUE))
  expect_error(drop_name(bib = test_bibfile, output_dir = 987, inline = TRUE))
  expect_error(drop_name(bib = test_bibfile, export_as = 1234, inline = TRUE))
  expect_error(drop_name(bib = test_bibfile, inline = "not logical"))
  expect_error(drop_name(bib = test_bibfile, max_authors = "not numeric", inline = TRUE))
  expect_error(drop_name(bib = test_bibfile, include_qr = "not logical", inline = TRUE))
  expect_error(drop_name(bib = test_bibfile, style = 987, inline = TRUE))
  expect_error(drop_name(bib = test_bibfile, substitute_missing = "not logical", inline = TRUE))
})
