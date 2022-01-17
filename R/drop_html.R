#' @title drop_html
#'
#' @description Accepts bibliographic information and returns a htmltools tagList for printing/display.
#'
#' @param work_item A data.frame or tibble with nrow(work_item) == 1 containing the
#' data for one reference to create the visual citation.
#' @param include_qr Character string specifying the way the QR code should be included or if no QR code should be included.
#' 'embed' results in a stand alone <img> tag within the HTML object, other options are ignored for the time being.
#' 'link' (default) creates a PNG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'link_svg' creates a SVG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'none' creates no QR code.
#' @param output_dir A string specifying the relative path, where the rendered output files should be stored.
#' @param style A string specifying the desired style for the visual citation. Possible values are:
#' "modern", "classic", "clean", "none". If "none" is given, the returned html can use a custom css file provided by the user.
#' This custom CSS file must specify styles for <div> classes "top-row", "title-row" and "author-row".
#' @param use_xaringan Boolean to specify if an HTML output is intended to be included in an HTML presentation (like e.g. xaringan) or not.
#' When including the visual citation via htmltools::includeHTML(), the QR code needs to be in a subfolder
#' relative to the rendered presentation, not relative to the visual citation.
#' @return A htmltools taglist containing the visual citation as HTML representation including style.
#'
#' @import htmltools


drop_html <- function(work_item, include_qr, output_dir, style, use_xaringan = FALSE) {

  # print(work_item)
  # CHECK ARGUMENTS
  stopifnot(is.character(work_item$TITLE) | is.na(work_item$TITLE))
  stopifnot(is.character(work_item$JOURNAL) | is.na(work_item$JOURNAL))
  stopifnot(is.character(work_item$authors_collapsed) | is.na(work_item$authors_collapsed))
  stopifnot(is.character(work_item$QR))
  stopifnot(is.character(include_qr))
  stopifnot(is.character(output_dir))
  stopifnot(is.character(style))
  stopifnot(is.logical(use_xaringan))

  # SUBSTITUTE MISSING
  if (is.na(work_item$TITLE)) {
    message(paste0("Empty title in: ", work_item$BIBTEXKEY))
    work_item$TITLE <- "Title missing"
  }
  if (is.na(work_item$JOURNAL)) {
    message(paste0("Empty JOURNAL in: ", work_item$BIBTEXKEY))
    work_item$JOURNAL <- "Unkown Journal"
  }
  if (is.na(work_item$YEAR)) {
    message(paste0("Empty YEAR in: ", work_item$BIBTEXKEY))
    work_item$YEAR <- "n.d."
  }
  if (is.na(work_item$authors_collapsed)) {
    message(paste0("Empty title in: ", work_item$BIBTEXKEY))
    work_item$AUTHOR <- "Unknown Author(s)"
  }

  # OBTAIN CSS STYLE
  css_styles <- get_css_styles(style = style)

  # define required QR output dir
  if (use_xaringan) {
    qr_dir <- here::here("qr")
  } else {
    qr_dir <- here::here(output_dir, "qr")
  }

  if (include_qr != "embed") {
    if (!dir.exists(qr_dir)) {
      tryCatch(
        expr = {
          dir.create(qr_dir)
        },
        error = function(e) {
          message("Could not create QR output folder:")
          print(e)
        },
        warning = function(w) {
          message("Having difficulties creating QR output folder:")
          print(w)
        }
      )
    }
  }

  if (style == "compact") {

    # COMPOSE "compact" HTML OBJECT

    htmltools::tagList(
      htmltools::tags$table(
        class = "visual-citation",
        htmltools::tags$tr(
          htmltools::tags$td(
            htmltools::div(
              class = "compact-author-row",
              style = css_styles$compact_author_row,
              # to shorten the author row, <br> tags can be included in the string
              # they are only rendered correctly, if the strings are interpreted via HTML()
              htmltools::tags$span(htmltools::HTML(work_item$authors_collapsed)),
            ),
            htmltools::div(
              htmltools::div(
                class = "compact-year-row",
                style = css_styles$compact_year_row,
                htmltools::tags$span(as.character(work_item$YEAR))
              )
            )
          ),
          htmltools::tags$td(
            if (include_qr == "embed") {
              if (capabilities("cairo")) {
                htmltools::plotTag(
                  plot(generate_qr(url = work_item$QR)),
                  alt = paste0("A QR code linking to the paper of ", work_item$authors_collapsed, " ", as.character(work_item$YEAR)),
                  device = grDevices::svg, width = 150, height = 150, pixelratio = 1 / 72,
                  mimeType = "image/svg+xml"
                )
              } else {
                message("Embedding as SVG is not supported on this device. Try setting up Cairo SVG properly if SVG is desired. Embedding as PNG.")
                htmltools::plotTag(
                  plot(generate_qr(url = work_item$QR)),
                  alt = paste0("A QR code linking to the paper of ", work_item$authors_collapsed, " ", as.character(work_item$YEAR)),
                  width = 150, height = 150
                )
              }
            } else if (include_qr == "link_svg") {
              if (capabilities("cairo")) {
                htmltools::capturePlot(
                  plot(generate_qr(url = work_item$QR)),
                  filename = here::here(qr_dir, paste0(work_item$BIBTEXKEY, ".svg")),
                  device = grDevices::svg, width = 2, height = 2
                )
                htmltools::tags$img(src = file.path("qr", paste0(work_item$BIBTEXKEY, ".svg")))
              } else {
                message("SVG export for QR not supported on this device. Try setting up Cairo SVG properly.")
              }
            } else if (include_qr == "link") {
              htmltools::capturePlot(
                plot(generate_qr(url = work_item$QR)),
                filename = here::here(qr_dir, paste0(work_item$BIBTEXKEY, "_qr.png")),
                width = 150, height = 150
              )
              htmltools::tags$img(src = file.path("qr", paste0(work_item$BIBTEXKEY, "_qr.png")), alt = "QR code")
            } else {
              message("No QR code will be created.")
            }
          )
        )
      )
    )
  } else {

    # COMPOSE "long" HTML OBJECT
    htmltools::tagList(
      htmltools::tags$table(
        class = "visual-citation",
        htmltools::tags$tr(
          htmltools::tags$td(
            htmltools::div(
              htmltools::div(
                class = "top-row",
                style = css_styles$top_row_style,
                htmltools::tags$span(paste0(work_item$JOURNAL, " (", as.character(work_item$YEAR), ")"))
              ),
              htmltools::div(
                class = "title-row",
                style = css_styles$title_row_style,
                htmltools::tags$span(work_item$TITLE)
              ),
              htmltools::div(
                class = "author-row",
                style = css_styles$author_row_style,
                htmltools::tags$span(work_item$authors_collapsed),
              )
            )
          ),
          htmltools::tags$td(
            if (include_qr == "embed") {
              if (capabilities("cairo")) {
                htmltools::plotTag(
                  plot(generate_qr(url = work_item$QR)),
                  alt = paste0("A QR code linking to the paper of ", work_item$authors_collapsed, " ", as.character(work_item$YEAR)),
                  device = grDevices::svg, width = 150, height = 150, pixelratio = 1 / 72,
                  mimeType = "image/svg+xml"
                )
              } else {
                message("Embedding as SVG is not supported on this device. Try setting up Cairo SVG properly if SVG is desired. Embedding as PNG.")
                htmltools::plotTag(
                  plot(generate_qr(url = work_item$QR)),
                  alt = paste0("A QR code linking to the paper of ", work_item$authors_collapsed, " ", as.character(work_item$YEAR)),
                  width = 150, height = 150
                )
              }
            } else if (include_qr == "link_svg") {
              if (capabilities("cairo")) {
                htmltools::capturePlot(
                  plot(generate_qr(url = work_item$QR)),
                  filename = here::here(qr_dir, paste0(work_item$BIBTEXKEY, ".svg")),
                  device = grDevices::svg, width = 2, height = 2
                )
                htmltools::tags$img(src = file.path("qr", paste0(work_item$BIBTEXKEY, ".svg")))
              } else {
                message("SVG export for QR not supported on this device. Try setting up Cairo SVG properly.")
              }
            } else if (include_qr == "link") {
              htmltools::capturePlot(
                plot(generate_qr(url = work_item$QR)),
                filename = here::here(qr_dir, paste0(work_item$BIBTEXKEY, "_qr.png")),
                width = 150, height = 150
              )
              htmltools::tags$img(src = file.path("qr", paste0(work_item$BIBTEXKEY, "_qr.png")), alt = "QR code")
            } else {
              message("No QR code will be created.")
            }
          )
        )
      )
    )
  }
}
