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
#' @export
#' @importFrom htmltools div img tags
#' @importFrom xfun base64_uri


drop_html <- function(title = "", journal = "", authors = "", year = "", cite_key = "", include_qr = TRUE) {

  # return warnings for missing data
  if (title == "") {
    warning("No title provided.")
  }
  if (journal == "") {
    warning("No Journal name provided.")
  }
  if (authors == "") {
    warning("No authors provided.")
  }
  if (year == "") {
    warning("No publication year provided.")
  }
  if (cite_key == "") {
    stop("No citation key provided! Check function call to privide the necessary cite_key argument.")
  }

  # style_dependency <- function() {
  #   htmltools::htmlDependency(
  #     name = "modern css",
  #     version = "0.1",
  #     src = "css/",
  #     stylesheet = "modern.css"
  #   )
  # }

  # style = paste("font-family: 'Helvetica', 'Arial', 'Verdana', sans-serif;font-weight: bold;"),




  vc <- htmltools::tagList(
    htmltools::tags$table(
      htmltools::tags$tr(
        htmltools::tags$td(
          htmltools::div(
            class = "visual-citation",
            htmltools::div(
              class = "top-row",
              style = paste("font-size: 1.3rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal;font-variant: small-caps;color:#ae2746;"),
              htmltools::tags$span(paste0(journal, " (", year, ")"))
            ),
            htmltools::div(
              class = "title-row",
              style = paste("font-size: 2rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: bold"),
              htmltools::tags$span(title)
            ),
            htmltools::div(
              class = "author-row",
              style = paste("font-size: 1.3rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal"),
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

  htmltools::html_print(vc)
  return(vc)
}

