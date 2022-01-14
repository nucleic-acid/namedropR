test_that("saving as html_full works as expected", {
  VCS <- htmltools::tagList(
    htmltools::div(
      htmltools::span("Lorem_ipsum")
    )
  )
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~vcs,
    "Some 2023", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2022", "2022", VCS
  )

  # writes a file

  expect_true(
    file.exists(write_vc(
      work_item = bib_tbl,
      export_as = "html_full",
      output_dir = tempdir(),
      path_absolute = FALSE
    ))
  )

  unlink(tempdir())

  expect_true(
    file.exists(
      write_vc(
        work_item = bib_tbl,
        export_as = "html_full",
        output_dir = tempdir(),
        path_absolute = TRUE
      )
    )
  )

  unlink(tempdir())

  # written file has a <!DOCTYPE html> tag in the beginning (indicating a 'full' HTML file)
  expect_equal(
    readLines(
      write_vc(
        work_item = bib_tbl,
        export_as = "html_full",
        output_dir = tempdir(),
        path_absolute = TRUE
      ),
      n = 1
    ),
    "<!DOCTYPE html>"
  )
  unlink(tempdir())
})


test_that("saving as html works as expected", {
  VCS <- htmltools::tagList(
    htmltools::div(
      htmltools::span("Lorem_ipsum")
    )
  )
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~vcs,
    "Some 2022", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2022", "2022", VCS
  )

  # writes a file
  expect_true(
    file.exists(write_vc(
      work_item = bib_tbl,
      export_as = "html",
      output_dir = tempdir(),
      path_absolute = FALSE
    ))
  )
  unlink(tempdir())

  expect_true(
    file.exists(
      write_vc(
        work_item = bib_tbl,
        export_as = "html",
        output_dir = tempdir(),
        path_absolute = TRUE
      )
    )
  )
  unlink(tempdir())

  # written file has a <div> tag in the beginning (indicating a 'not-full' HTML file)
  expect_equal(
    readLines(
      write_vc(
        work_item = bib_tbl,
        export_as = "html",
        output_dir = tempdir(),
        path_absolute = FALSE
      ),
      n = 1
    ),
    "<div>"
  )
  unlink(tempdir())
})

test_that("saving as png works as expected", {
  VCS <- htmltools::tagList(
    htmltools::div(
      class = "visual-citation",
      htmltools::span("Lorem_ipsum")
    )
  )
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~vcs,
    "Some 2023", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2023", "2023", VCS
  )

  # writes a file


  expect_true(
    file.exists(write_vc(
      work_item = bib_tbl,
      export_as = "png",
      output_dir = tempdir(),
      path_absolute = FALSE
    ))
  )
  unlink(tempdir())

  expect_true(
    file.exists(
      write_vc(
        work_item = bib_tbl,
        export_as = "png",
        output_dir = tempdir(),
        path_absolute = TRUE
      )
    )
  )
  unlink(tempdir())

  # intermediate html_full is deleted
  expect_false(
    file.exists(
      gsub(
        ".png",
        "",
        write_vc(
          work_item = bib_tbl,
          export_as = "png",
          output_dir = tempdir(),
          path_absolute = FALSE
        ),
        fixed = TRUE
      )
    )
  )
  unlink(tempdir())
})

test_that("CSS class is assigned for webshot to find", {
  VCS <- htmltools::tagList(
    htmltools::div(
      class = "wrong-class",
      htmltools::span("Lorem_ipsum")
    )
  )
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~vcs,
    "Some 2023", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2023", "2023", VCS
  )


  # fails due to phantomJS error, so catching the error within R seems impossible:
  run1 <- evaluate_promise(
        write_vc(
        work_item = bib_tbl,
        export_as = "png",
        output_dir = tempdir(),
        path_absolute = TRUE
    )
  )

  expect_true(grep("Could not take a screenshot", run1$messages, fixed = TRUE) >= 1)
  expect_true(grep("PHANTOM ERROR", run1$output, fixed = TRUE) >= 1)
})

test_that("unknown output format is caught", {
  VCS <- htmltools::tagList(
    htmltools::div(
      class = "visual-citation",
      htmltools::span("Lorem_ipsum")
    )
  )
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~vcs,
    "Some 2023", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2023", "2023", VCS
  )

  expect_error(
    write_vc(
      work_item = bib_tbl,
      export_as = "wrong_format",
      output_dir = tempdir(),
      path_absolute = FALSE
    )
  )
  unlink(tempdir())
})
