# CHECK INPUTS
test_that("Bibliographies are correctly read ", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  empty_path <- system.file("testdata", "empty.bib", package = "namedropR")
  empty_bibfile <- RefManageR::ReadBib(empty_path)
  expect_message(drop_name(bib = bib_path, cite_key = "Eschrich1983", inline = TRUE), "Bibliography file successfully read")
  expect_error(drop_name(bib = "wrong_file_path", cite_key = "Eschrich1983"), "file not found")
  expect_error(drop_name(bib = empty_bibfile, cite_key = "Eschrich1983", "is empty"))
  expect_error(drop_name(bib = 123, inline = TRUE))
  expect_error(drop_name(cite_key = "Eschrich1983", inline = TRUE), "No bibliography")
})

test_that("backwards compatibility for bibtex", {
  new_bib <- RefManageR::BibEntry(
    bibtype = "Article", key = "back1", title = "An Article Title",
    author = "The Author", journaltitle = "The Journal Title",
    date = "2014-02-06", pubstate = "forthcoming", doi = "124567890"
  )
  expect_type(drop_name(new_bib, cite_key = "back1"), "character")
})

test_that("message is issued, when no DOI/URL is available", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_message(drop_name(bib = test_bibfile, cite_key = "HAWKING_noURL"))
})

test_that("Citation key is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, cite_key = "wrong_cite_key"), "entry not found")
  expect_error(drop_name(bib = test_bibfile, cite_key = 5))
})

test_that("output_dir is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, output_dir = TRUE))
  expect_error(drop_name(bib = test_bibfile, output_dir = 5))
})

test_that("export_as is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, export_as = "no export option"))
  expect_error(drop_name(bib = test_bibfile, cite_key = 5))
})

test_that("max_authors is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, max_authors = "many authors"))
  expect_error(drop_name(bib = test_bibfile, max_authors = -1))
})

test_that("inline is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, inline = "5"))
})

test_that("inline = TRUE returns a non-empty tagList", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_true(any(class(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", export_as = "html", inline = TRUE)) == "shiny.tag.list"))
  expect_true(length(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", export_as = "html", inline = TRUE)) > 0)
})

test_that("inline = FALSE returns a correct file path as character", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_type(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", inline = FALSE), "character")
  expect_true(file.exists(here::here(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", inline = FALSE))))
})

test_that("include_qr is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, include_qr = "invalid option"))
  expect_error(drop_name(bib = test_bibfile, include_qr = 5))
})

test_that("style is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, style = 987))
})

test_that("substitute_missing is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, substitute_missing = 987))
})

test_that("use_xaringan is properly checked", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
  expect_error(drop_name(bib = test_bibfile, use_xaringan = 987))
  unlink(here::here("visual_citations"), recursive = TRUE)
})


# TECHNICAL OUTPUT CHECKS
test_that("use_xaringan creates the correct folders (qr/ in working dir)", {
  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))

  drop_name(bib = test_bibfile, export_as = "html", cite_key = "collaboration_2019_ApJL", use_xaringan = TRUE)
  expect_true(dir.exists(here::here("visual_citations")))
  expect_true(dir.exists(here::here("qr")))

  unlink(here::here("visual_citations"), recursive = TRUE)
  unlink(here::here("qr"), recursive = TRUE)

  drop_name(bib = test_bibfile, export_as = "html_full", cite_key = "collaboration_2019_ApJL", use_xaringan = TRUE)
  expect_true(dir.exists(here::here("visual_citations")))
  expect_true(dir.exists(here::here("qr")))

  unlink(here::here("visual_citations"), recursive = TRUE)
  unlink(here::here("qr"), recursive = TRUE)
})
