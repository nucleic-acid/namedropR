test_bibfile <- system.file("testdata", "sample.bib", package="namedropR")
empty_bibfile <- system.file("testdata", "empty.bib", package="namedropR")

test_that("inline = TRUE returns the correct objects", {
  expect_type(drop_name(bib_file = test_bibfile, cite_key = "Eschrich1983", export_as = "html", inline = TRUE), "list")
})

test_that("inline = FALSE returns the correct file path as character", {
  expect_type(drop_name(bib_file = test_bibfile, cite_key = "Eschrich1983", inline = FALSE), "character")
  expect_true(file.exists(drop_name(bib_file = test_bibfile, cite_key = "Eschrich1983", inline = FALSE)))
})

test_that("inline = FALSE produces the correct output filetypes", {
  expect_equal(readLines(drop_name(bib_file = test_bibfile, cite_key = "Eschrich1983", export_as = "html", inline = FALSE), n = 1),
               "<!DOCTYPE html>")
  unlink(here::here("visual_citations"), recursive = TRUE)
})


test_that("reading and extracting bibtex information is error prone", {
  expect_error(drop_name(bib_file = "wrong_file_path", cite_key = "Eschrich1983"), "file not found")
  expect_error(drop_name(bib_file = test_bibfile, cite_key = "wrong_cite_key"), "BibTeX entry not found")
  expect_error(drop_name(bib_file = empty_bibfile, cite_key = "Eschrich1983", "file is empty"))
})

test_that("input argument types are correct", {
  expect_error(drop_name(bib_file = 123))
  expect_error(drop_name(bib_file = test_bibfile, cite_key = 1234))
  expect_error(drop_name(bib_file = test_bibfile, output_dir = 987))
  expect_error(drop_name(bib_file = test_bibfile, export_as = 1234))
  expect_error(drop_name(bib_file = test_bibfile, inline = "not logical"))
  expect_error(drop_name(bib_file = test_bibfile, max_authors = "not numeric"))
  expect_error(drop_name(bib_file = test_bibfile, include_qr = "not logical"))
  expect_error(drop_name(bib_file = test_bibfile, style = 987))
  expect_error(drop_name(bib_file = test_bibfile, substitute_missing = "not logical"))
})
