test_that("vectors are returned as single string", {
  authors <- c("Doe, J.", "Sample, Simon", "Free, Fred", "Random, Ronald")
  expect_true(length(manage_authors(authors = authors, max_authors = 4)) == 1)
})

test_that("vectors are returned as string with correct length", {
  authors <- c("Doe, J.", "Sample, Simon", "Free, Fred", "Random, Ronald")
  expect_true(nchar(manage_authors(authors = authors, max_authors = 3)) == 42)
})

test_that("only cropped vectors end with 'et. al.'", {
  authors <- c("Doe, John", "Sample, Simon", "Fancy, Fred", "Random, Ronald")
  expect_true(grepl("et\\.\\sal\\.$", manage_authors(authors = authors, max_authors = 3)))
  expect_true(grepl("et\\.\\sal\\.$", manage_authors(authors = authors, max_authors = 1)))
  expect_false(grepl("et\\.\\sal\\.$", manage_authors(authors = authors, max_authors = 4)))
})

test_that("also lists are accepted", {
  authors_list <- as.list(c("Doe, John", "Sample, Simon", "Fancy, Fred", "Random, Ronald"))
  expect_true(length(manage_authors(authors = authors_list, max_authors = 3)) == 1)
})
