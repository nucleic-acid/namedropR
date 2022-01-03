test_bibfile <- system.file("testdata", "sample.bib", package="namedropR")
if (file.exists(test_bibfile)) {
  bib <- bibtex::read.bib(file = test_bibfile)
}

test_that("missing data is substituted correctly", {
  expect_warning(check_bib_complete(substitute_missing = TRUE, bib["HAWKING_noYEAR"]))
  expect_warning(check_bib_complete(substitute_missing = TRUE, bib["HAWKING_noTITLE"]))
  expect_warning(check_bib_complete(substitute_missing = TRUE, bib["HAWKING_noAUTHOR"]))
  expect_warning(check_bib_complete(substitute_missing = TRUE, bib["HAWKING_noJOURNAL"]))
})

test_that("function returns a bibentry", {
  expect_true(class(check_bib_complete(substitute_missing = TRUE, bib["Eschrich1983"]))== "bibentry")
  expect_true(class(check_bib_complete(substitute_missing = FALSE, bib["Eschrich1983"]))== "bibentry")
})

test_that("returned bibentry contains all necessary fields", {
  temp_return_sub <- suppressWarnings(check_bib_complete(substitute_missing = TRUE, bib["HAWKING_noYEAR"]))
  expect_true(!is.null(temp_return_sub$title))
  expect_true(!is.null(temp_return_sub$author))
  expect_true(!is.null(temp_return_sub$journal))
  expect_true(!is.null(temp_return_sub$year))

  temp_return_no_sub <- suppressWarnings(check_bib_complete(substitute_missing = FALSE, bib["HAWKING_noNOTHING"]))
  expect_true(!is.null(temp_return_no_sub$title))
  expect_true(!is.null(temp_return_no_sub$author))
  expect_true(!is.null(temp_return_no_sub$journal))
  expect_true(!is.null(temp_return_no_sub$year))
})

test_that("valid fields are not replaced", {
  valid_entry_return <- suppressWarnings(check_bib_complete(substitute_missing = FALSE, bib["Eschrich1983"]))
  expect_equal(valid_entry_return, bib["Eschrich1983"])
})
