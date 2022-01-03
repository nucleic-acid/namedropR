#' @title drop_html
#'
#' @description Accepts bibliographic information and retuns a htmltools taglist for printing/display.
#'
#' @param journal The Journal's name as string.
#' @param title The publication's title as string.
#' @param authors The authors' names as string. If this is a list, it has to be collapsed to a single string (s. example).
#' @param year The publication's publication year as string.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param include_qr Boolean value, whether to include a QR code (containing the URL to the DOI) next to the visual citation.
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
#' @importFrom htmltools div img tags
#' @importFrom xfun base64_uri


drop_html <- function(title, journal, authors, year, cite_key, include_qr, style) {

  # CHECK ARGUMENTS
  stopifnot(class(title) == "character")
  stopifnot(class(journal) == "character")
  stopifnot(class(authors) == "character")
  stopifnot(class(year) == "character")
  stopifnot(class(cite_key) == "character")
  if (cite_key == "") {
    stop("No citation key provided! Check function call to privide the necessary cite_key argument.")
  }

  stopifnot(class(include_qr) == "logical")
  stopifnot(class(style) == "character")


  # OBTAIN CSS STYLE
  css_styles <- get_css_styles(style = style)



  vc <- htmltools::tagList(
    htmltools::tags$table(
      htmltools::tags$tr(
        htmltools::tags$td(
          htmltools::div(
            class = "visual-citation",
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
          if (include_qr) {
            htmltools::img(
              src = xfun::base64_uri(paste0(tempdir(), "/qr_codes/qr_", cite_key, ".svg")),
              alt = "qrcode",
              style = "float: center;padding-left:20px;height:150px;width:150px;"
            )
          }
        )
      )
    )
  )

  # htmltools::html_print(vc)
  return(vc)
}

