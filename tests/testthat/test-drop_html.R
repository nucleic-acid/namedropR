test_that("an HTML taglist is being returned with all putput options", {
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~QR,
    "Some 2022", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2022", "2022", "https://en.wikipedia.org"
  )

  if (!file.exists(tempdir())) {
    dir.create(tempdir())
  }

  expect_equal(
    class(drop_html(
      work_item = bib_tbl, include_qr = "link",
      output_dir = tempdir(), style = "modern",
      use_xaringan = FALSE
    )),
    class(htmltools::tagList())
  )

  expect_equal(
    class(drop_html(
      work_item = bib_tbl, include_qr = "link_svg",
      output_dir = tempdir(), style = "modern",
      use_xaringan = FALSE
    )),
    class(htmltools::tagList())
  )

  expect_equal(
    class(drop_html(
      work_item = bib_tbl, include_qr = "embed",
      output_dir = tempdir(), style = "modern",
      use_xaringan = FALSE
    )),
    class(htmltools::tagList())
  )

  expect_equal(
    class(suppressMessages(drop_html(
      work_item = bib_tbl, include_qr = "none",
      output_dir = tempdir(), style = "modern",
      use_xaringan = FALSE
    ))),
    class(htmltools::tagList())
  )
})

test_that("inputs are correct", {
  if (!file.exists(tempdir())) {
    dir.create(tempdir())
  }

  wrong_title <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~QR,
    12345, "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2022", "2022", "https://en.wikipedia.org",
    NA, "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2023", "2022", "https://en.wikipedia.org",
  )

  expect_error(drop_html(
    wrong_title[1, ],
    include_qr = "embed",
    output_dir = tempdir(), style = "modern", use_xaringan = FALSE
  ))

  expect_message(drop_html(
    wrong_title[2, ],
    include_qr = "embed",
    output_dir = tempdir(), style = "modern", use_xaringan = FALSE
  ))

  wrong_year <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~QR,
    "Some 2022", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2022", "12345", "https://en.wikipedia.org",
    "Some 2022", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2023", NA, "https://en.wikipedia.org"
  )
  # YEAR is alredy checked and converted to character in drop_name()

  expect_message(drop_html(
    wrong_year[2, ],
    include_qr = "embed",
    output_dir = tempdir(), style = "modern", use_xaringan = FALSE
  ))

  wrong_authors <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~QR,
    "Some 2022", 12345, "Journal of Unnecessary R Packages", "Alice2022", "2022", "https://en.wikipedia.org",
    "Some 2022", NA, "Journal of Unnecessary R Packages", "Alice2023", "2022", "https://en.wikipedia.org",
  )

  expect_error(drop_html(
    wrong_authors[1, ],
    include_qr = "embed",
    output_dir = tempdir(), style = "modern", use_xaringan = FALSE
  ))
  expect_message(drop_html(
    wrong_authors[2, ],
    include_qr = "embed",
    output_dir = tempdir(), style = "modern", use_xaringan = FALSE
  ))

  wrong_journal <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~QR,
    "Some 2022", "Alice; Bob; Charlie", 12345, "Alice2022", "2022", "https://en.wikipedia.org",
    "Some 2022", "Alice; Bob; Charlie", NA, "Alice2023", "2022", "https://en.wikipedia.org",
  )

  expect_error(drop_html(
    wrong_journal[1, ],
    include_qr = "embed",
    output_dir = tempdir(), style = "modern", use_xaringan = FALSE
  ))
  expect_message(drop_html(
    wrong_journal[2, ],
    include_qr = "embed",
    output_dir = tempdir(), style = "modern", use_xaringan = FALSE
  ))
})

test_that("use_xaringan sets the right folder-structure", {
  bib_tbl <- dplyr::tribble(
    ~TITLE, ~authors_collapsed, ~JOURNAL, ~BIBTEXKEY, ~YEAR, ~QR,
    "Some 2022", "Alice; Bob; Charlie", "Journal of Unnecessary R Packages", "Alice2022", "2022", "https://en.wikipedia.org"
  )

  unlink(tempdir())
  if (!file.exists(file.path(tempdir()))) {
    dir.create(file.path(tempdir()))
  }
  if (!file.exists(file.path(tempdir(), "visual_citations"))) {
    dir.create(file.path(tempdir(), "visual_citations"))
  }

  temp_output <- drop_html(
    bib_tbl,
    include_qr = "link",
    output_dir = file.path(tempdir(), "visual_citations"),
    style = "modern",
    use_xaringan = FALSE
  )

  expect_true(
    file.exists(file.path(tempdir(), "visual_citations", "qr"))
  )

  temp_output <- drop_html(
    bib_tbl,
    include_qr = "link",
    output_dir = file.path(tempdir(), "visual_citations"),
    style = "modern",
    use_xaringan = TRUE
  )

  expect_true(
    file.exists(file.path(tempdir(), "qr"))
  )

  unlink(here::here("qr"))
})
