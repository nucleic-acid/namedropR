test_that("Correct output format", {
  expect_type(get_css_styles(style = "modern"), "list")
})

test_that("Checking returned list length", {
  expect_length(get_css_styles(style = "modern"), 3)
  expect_length(get_css_styles(style = "classic"), 3)
  expect_length(get_css_styles(style = "clean"), 3)
  expect_length(get_css_styles(style = "compact"), 2)
  expect_length(suppressWarnings(get_css_styles(style = "none")), 3)
})

test_that("Checking warning messages are in place", {
  expect_warning(get_css_styles(style = "wrong_arg"), "not defined")
})

test_that("style table has unique names", {
  style_df <- readr::read_csv(system.file("styles", "styles.csv", package = "namedropR"), show_col_types = FALSE)
  expect_false(any(dplyr::count(style_df, .data$style_name)$n > 1))
})

test_that("no style masks the 'compact' style", {
  style_df <- readr::read_csv(system.file("styles", "styles.csv", package = "namedropR"), show_col_types = FALSE)
  expect_false(any("compact" %in% style_df$style_name))
})
