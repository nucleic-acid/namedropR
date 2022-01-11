# CHECK INPUTS
test_that("Bibliographies are correctly read ", {
  empty_bib <- dplyr::tribble(
    ~TITLE, ~AUTHOR, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~DOI
  )
  expect_error(drop_name(bib = empty_bib, cite_key = "Eschrich1983"), "is empty")

  bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
  expect_error(drop_name(bib = "wrong_file_path", cite_key = "Eschrich1983"), "file not found")
  expect_error(drop_name(bib = 123), "Inappropriate type of bibliography")
  expect_error(drop_name(cite_key = "Eschrich1983"), "No bibliography")

  good_bib <- system.file("testdata", "good.bib", package = "namedropR")
  expect_message(
    expect_message(
      drop_name(bib = good_bib, cite_key = "Eschrich1983"),
      "Years coerced to string format."
      ),
    "Bibliography file successfully read."
    )
})

test_that("compatibility for biblatex works", {
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~AUTHOR, ~JOURNALTITLE, ~BIBTEXKEY, ~YEAR, ~DOI,
    "Some 2022", c("Alice", "Bob", "Charlie"), "Journal of Unnecessary R Packages", "Alice2022", "2022", "https://en.wikipedia.org"
  )
  expect_message(drop_name(bib = bib_tbl, cite_key = "Alice2022"), "BibLaTeX field 'JOURNALTITLE")

  bib_tbl <- dplyr::tribble(
    ~TITLE, ~AUTHOR, ~JOURNAL, ~BIBTEXKEY, ~DATE, ~DOI,
    "Some 2022", c("Alice", "Bob", "Charlie"), "Journal of Unnecessary R Packages", "Alice2022", "2022", "https://en.wikipedia.org"
  )
  expect_message(drop_name(bib = bib_tbl, cite_key = "Alice2022"), "BibLaTeX field 'DATE")
})

test_that("DATEs are read correctly and errors are caught", {
  date_variants <- dplyr::tribble(
    ~TITLE, ~AUTHOR, ~JOURNAL, ~BIBTEXKEY, ~DATE, ~DOI,
    "Some 2022", c("Alice", "Bob", "Charlie"), "Journal of Unnecessary R Packages", "Alice2022", "2022-10-29", "https://en.wikipedia.org",
    "Some 2022", c("Alice", "Bob", "Charlie"), "Journal of Unnecessary R Packages", "Alice2022", "2022-11", "https://en.wikipedia.org",
    "Some 2022", c("Alice", "Bob", "Charlie"), "Journal of Unnecessary R Packages", "Alice2022", "2022", "https://en.wikipedia.org",
    "Some 2022", c("Alice", "Bob", "Charlie"), "Journal of Unnecessary R Packages", "Alice2022", "August 206", "https://en.wikipedia.org"
  )
  expect_message(drop_name(bib = date_variants[1,], cite_key = "Alice2022"), "BibLaTeX field 'DATE")
  expect_message(drop_name(bib = date_variants[2,], cite_key = "Alice2022"), "BibLaTeX field 'DATE")
  expect_message(drop_name(bib = date_variants[3,], cite_key = "Alice2022"), "BibLaTeX field 'DATE")
  expect_warning(suppressMessages(drop_name(bib = date_variants[4,], cite_key = "Alice2022")))


})

test_that("all required columns are enforced", {
  missing_cols <- dplyr::tribble(
    ~TITLE, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~DOI,
    "One", "Three", "Four", "2021", "Six"
    )

  # Authors missing
  expect_error(drop_name(missing_cols, cite_key = "Four"))

})


# test_that("QR-URL generation is properly handled", {
#   qr_variants <- dplyr::tribble(
#     ~TITLE, ~AUTHOR, ~JOURNAL, ~BIBTEXKEY, ~DATE, ~DOI, ~URL,
#     "Some 2022", "Alice", "JoURP", "hasDOI", "2022-10-29", "10.fancyDOI/something", "https://en.wikipedia.org",
#     "Some 2022", "Alice", "JoURP", "noDOI", "2022-11", NA, "https://en.wikipedia.org",
#     "Some 2022", "Alice", "JoURP", "noDOInoURL", "2022", NA, NA
#   )
#   })



test_that("bulk operations work properly", {
  bulk_data <- dplyr::tribble(
    ~TITLE, ~AUTHOR, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~DOI,
    "Title1", c("Alice1", "Bob1", "Charlie1"), "JoURP1", "Alice2021", "2021", "someDOI1",
    "Title2", c("Alice2", "Bob2", "Charlie2"), "JoURP2", "Alice2022", "2022", "someDOI2",
    "Title3", c("Alice3", "Bob3", "Charlie3"), "JoURP3", "Alice2023", "2023", "someDOI3",
    "Title4", c("Alice4", "Bob4", "Charlie4"), "JoURP4", "Alice2024", "2024", "someDOI4"
  )

  # no cite_key given, 4 VCs should be created/4 paths returned
  bulk_res1 <- evaluate_promise(drop_name(bib = bulk_data, output_dir = tempdir()))
  expect_equal(length(bulk_res1$result), 4)
  expect_message(drop_name(bib = bulk_data, output_dir = tempdir()), "No cite_key specified.")

  # vector of 2 cite_keys returns 2 paths
  bulk_res2 <- evaluate_promise(drop_name(bib = bulk_data, cite_key = c("Alice2021", "Alice2023"), output_dir = tempdir()))
  expect_equal(length(bulk_res2$result), 2)

  # vector of 3 cite_keys with one false key returns 2, throws warning
  bulk_res3 <- evaluate_promise(drop_name(bib = bulk_data, cite_key = c("Alice2022", "Alice2024", "Bob2019"), output_dir = tempdir()))
  expect_equal(length(bulk_res3$result), 2)
  expect_true(bulk_res3$warnings == "The following cite_key items were not found in the provided library: Bob2019")

  # vector of only 3 false keys is supplied
  bulk_res4 <- evaluate_promise(drop_name(bib = bulk_data, cite_key = c("Bob1", "Bob2", "Bob3"), output_dir = tempdir()))
  expect_equal(bulk_res4$warnings[2], "No reference matches the given cite_keys. Please check that citation key(s) are correct.")

  # vector of type 'numerical' throws an error
  expect_error(drop_name(bib = bulk_data, cite_key = c(1, 1, 2, 3, 5, 8), output_dir = tempdir()), "cite_key must be of type 'caracter'")

  # library with non-unique BIBTEXKEYs
  bulk_data_dup <- dplyr::tribble(
    ~TITLE, ~AUTHOR, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~DOI,
    "Title1", c("Alice1", "Bob1", "Charlie1"), "JoURP1", "Alice2021", "2021", "someDOI1",
    "Title1", c("Alice2", "Bob2", "Charlie2"), "JoURP1", "Alice2021", "2021", "someDOI3",
  )

  expect_warning(drop_name(bib = bulk_data_dup, cite_key = "Alice2021", output_dir = tempdir()), "BIBTEX keys are not unique")
})
#
# test_that("output_dir is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, output_dir = TRUE))
#   expect_error(drop_name(bib = test_bibfile, output_dir = 5))
# })
#
# test_that("export_as is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, export_as = "no export option"))
#   expect_error(drop_name(bib = test_bibfile, cite_key = 5))
# })
#
# test_that("max_authors is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, max_authors = "many authors"))
#   expect_error(drop_name(bib = test_bibfile, max_authors = -1))
# })
#
# test_that("inline is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, inline = "5"))
# })
#
# test_that("inline = TRUE returns a non-empty tagList", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_true(any(class(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", export_as = "html", inline = TRUE)) == "shiny.tag.list"))
#   expect_true(length(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", export_as = "html", inline = TRUE)) > 0)
# })
#
# test_that("inline = FALSE returns a correct file path as character", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_type(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", inline = FALSE), "character")
#   expect_true(file.exists(here::here(drop_name(bib = test_bibfile, cite_key = "Eschrich1983", inline = FALSE))))
# })
#
# test_that("include_qr is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, include_qr = "invalid option"))
#   expect_error(drop_name(bib = test_bibfile, include_qr = 5))
# })
#
# test_that("style is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, style = 987))
# })
#
# test_that("substitute_missing is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, substitute_missing = 987))
# })
#
# test_that("use_xaringan is properly checked", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#   expect_error(drop_name(bib = test_bibfile, use_xaringan = 987))
#   unlink(here::here("visual_citations"), recursive = TRUE)
# })
#
#
# # TECHNICAL OUTPUT CHECKS
# test_that("use_xaringan creates the correct folders (qr/ in working dir)", {
#   bib_path <- system.file("testdata", "sample.bib", package = "namedropR")
#   test_bibfile <- suppressMessages(RefManageR::ReadBib(bib_path))
#
#   drop_name(bib = test_bibfile, export_as = "html", cite_key = "collaboration_2019_ApJL", use_xaringan = TRUE)
#   expect_true(dir.exists(here::here("visual_citations")))
#   expect_true(dir.exists(here::here("qr")))
#
#   unlink(here::here("visual_citations"), recursive = TRUE)
#   unlink(here::here("qr"), recursive = TRUE)
#
#   drop_name(bib = test_bibfile, export_as = "html_full", cite_key = "collaboration_2019_ApJL", use_xaringan = TRUE)
#   expect_true(dir.exists(here::here("visual_citations")))
#   expect_true(dir.exists(here::here("qr")))
#
#   unlink(here::here("visual_citations"), recursive = TRUE)
#   unlink(here::here("qr"), recursive = TRUE)
# })
