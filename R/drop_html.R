#' @title drop_html
#'
#' @description Accepts bibliographic information and returns a htmltools tagList for printing/display.
#'
#' @param journal The Journal's name as string.
#' @param title The publication's title as string.
#' @param authors The authors' names as string. If this is a list, it has to be collapsed to a single string (s. example).
#' @param year The publication's publication year as string.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param include_qr Character string specifying the way the QR code should be included or if no QR code should be included.
#' 'embed' results in a stand alone <img> tag within the HTML object, other options are ignored for the time being.
#' 'link' (default) creates a PNG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'link_svg' creates a SVG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'none' creates no QR code.
#' @param output_dir A string specifying the relative path, where the rendered output files should be stored.
#' @param url The URL that should be encoded as QR code.
#' @param style A string specifying the desired style for the visual citation. Possible values are:
#' "modern", "classic", "clean", "none". If "none" is given, the returned html can use a custom css file provided by the user.
#' This custom CSS file must specify styles for <div> classes "top-row", "title-row" and "author-row".
#' @return A htmltools taglist containing the visual citation as HTML representation including style.
#'
#' @examples
#' \dontrun{
#' vc_html <- drop_html(
#'   title = bibentry$title,
#'   cite_key = bibentry$key,
#'   journal = bibentry$journal,
#'   year = bibentry$year,
#'   authors = paste(bibentry$author, collapse = ", ")
#' )
#'
#' htmltools::html_print(dropped)
#' }
#'
#' @import htmltools


drop_html <- function(title, journal, authors, year, cite_key, url, include_qr, output_dir, style) {

  # CHECK ARGUMENTS
  stopifnot(class(title) == "character")
  stopifnot(class(journal) == "character")
  stopifnot(class(authors) == "character")
  stopifnot(class(year) == "character")
  stopifnot(class(url) == "character")
  stopifnot(class(include_qr) == "character")
  stopifnot(class(cite_key) == "character")
  if (cite_key == "") {
    stop("No citation key provided! Check function call to privide the necessary cite_key argument.")
  }

  stopifnot(class(style) == "character")


  # OBTAIN CSS STYLE
  css_styles <- get_css_styles(style = style)


  # COMPOSE HTML OBJECT
  vc <- htmltools::tagList(
    htmltools::tags$table(
      htmltools::tags$tr(
        class = "visual-citation",
        htmltools::tags$td(
          htmltools::div(
            htmltools::div(
              class = "top-row",
              style = css_styles$top_row_style,
              htmltools::tags$span(paste0(journal, " (", year, ")"))
            ),
            htmltools::div(
              class = "title-row",
              style = css_styles$title_row_style,
              htmltools::tags$span(title)
            ),
            htmltools::div(
              class = "author-row",
              style = css_styles$author_row_style,
              htmltools::tags$span(authors),
            )
          )
        ),
        htmltools::tags$td(
          if (include_qr == "embed") {
            if (capabilities("cairo")) {
              htmltools::plotTag(
                plot(generate_qr(url = url)),
                alt = paste0("A QR code linking to the paper of ", authors, " ", year),
                device = grDevices::svg, width = 150, height = 150, pixelratio = 1 / 72,
                mimeType = "image/svg+xml"
              )
            } else {
              message("Embedding as PNG as SVG is not supported on this device. Try setting up Cairo SVG properly if SVG is desired.")
              htmltools::plotTag(
                plot(generate_qr(url = url)),
                alt = paste0("A QR code linking to the paper of ", authors, " ", year),
                width = 150, height = 150
              )
            }
          } else if (include_qr == "link_svg") {
            if (capabilities("cairo")) {
              if (!dir.exists(here::here(output_dir, "qr"))) {
                message("qr dir needs to be created")
                dir.create(here::here(output_dir, "qr"))
              }
              htmltools::capturePlot(
                plot(generate_qr(url = url)),
                filename = here::here(output_dir, "qr", paste0(cite_key, ".svg")),
                device = grDevices::svg, width = 2, height = 2
              )
              htmltools::tags$img(src = file.path("qr", paste0(cite_key, ".svg")))
            } else {
              message("SVG export for QR not supported on this device. Try setting up Cairo SVG properly.")
            }
          } else if (include_qr == "link") {
            if (!dir.exists(here::here(output_dir, "qr"))) {
              # message("qr dir needs to be created")
              dir.create(here::here(output_dir, "qr"))
            }
            htmltools::capturePlot(
              plot(generate_qr(url = url)),
              filename = here::here(output_dir, "qr", paste0(cite_key, "_qr.png")),
              width = 150, height = 150
            )
            htmltools::tags$img(src = file.path("qr", paste0(cite_key, "_qr.png")), alt = "QR code")
          } else {
            message("No QR code will be created.")
          }
        )
      )
    )
  )
}
