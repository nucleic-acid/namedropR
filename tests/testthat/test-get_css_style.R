test_that("Correct output format", {
  expect_type(get_css_styles(style = "modern"), "list")
})

test_that("Checking returned list length", {
  expect_length(get_css_styles(style = "modern"), 3)
  expect_length(get_css_styles(style = "classic"), 3)
  expect_length(get_css_styles(style = "clean"), 3)
  expect_length(get_css_styles(style = "none"), 3)
})

test_that("Checking warning messages are in place", {
  expect_warning(get_css_styles(style = "wrong_arg"), "not defined")
})
