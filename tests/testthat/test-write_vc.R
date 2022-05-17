test_that("saving as html_full works as expected", {
  new <- tempdir(check = TRUE) # check and (if needed) create temp dir before running this test

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
  withr::with_dir(
    new = new,
    code = {
      expect_true(
        file.exists(write_vc(
          work_item = bib_tbl,
          export_as = "html_full",
          output_dir = "visual_citations5",
          path_absolute = FALSE
        ))
      )
    }
  )

  withr::with_dir(
    new = new,
    code = {
      expect_true(
        file.exists(
          write_vc(
            work_item = bib_tbl,
            export_as = "html_full",
            output_dir = "visual_citations6",
            path_absolute = TRUE
          )
        )
      )
    }
  )

  # written file has a <!DOCTYPE html> tag in the beginning (indicating a 'full' HTML file)
  withr::with_dir(
    new = new,
    code = {
      expect_equal(
        readLines(
          write_vc(
            work_item = bib_tbl,
            export_as = "html_full",
            output_dir = "visual_citations7",
            path_absolute = TRUE
          ),
          n = 1
        ),
        "<!DOCTYPE html>"
      )
    }
  )
})


test_that("saving as html works as expected", {
  new <- tempdir(check = TRUE)

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
  withr::with_dir(
    new = new,
    code = {
      expect_true(
        file.exists(write_vc(
          work_item = bib_tbl,
          export_as = "html",
          output_dir = "visual_citations4",
          path_absolute = FALSE
        ))
      )
    }
  )

  withr::with_dir(
    new = new,
    code = {
      expect_true(
        file.exists(
          write_vc(
            work_item = bib_tbl,
            export_as = "html",
            output_dir = "visual_citations3",
            path_absolute = TRUE
          )
        )
      )
    }
  )

  # written file has a <div> tag in the beginning (indicating a 'not-full' HTML file)

  withr::with_dir(
    new = new,
    code = {
      expect_equal(
        readLines(
          write_vc(
            work_item = bib_tbl,
            export_as = "html",
            output_dir = "visual_citations0",
            path_absolute = FALSE
          ),
          n = 1
        ),
        "<div>"
      )
    }
  )
})

test_that("saving as png works as expected", {
  new <- tempdir(check = TRUE)

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

  # writes a file, this test can only be run, if phantomJS is installed:
  if(webshot::is_phantomjs_installed()) {
    withr::with_dir(
      new = new,
      code = {
        expect_true(
          file.exists(
            write_vc(
              work_item = bib_tbl,
              export_as = "png",
              output_dir = "visual_citations1",
              path_absolute = FALSE
            )
          )
        )
      }
    )

    withr::with_dir(
      new = new,
      code = {
        expect_true(
          file.exists(
            write_vc(
              work_item = bib_tbl,
              export_as = "png",
              output_dir = "visual_citations2",
              path_absolute = TRUE
            )
          )
        )
      }
    )
  }

})


test_that("unknown output format is caught", {
  new <- tempdir(check = TRUE)

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

  withr::with_dir(
    new = new,
    code = {
      expect_error(
        write_vc(
          work_item = bib_tbl,
          export_as = "wrong_format",
          output_dir = "visual_citations",
          path_absolute = FALSE
        )
      )
    }
  )
})
