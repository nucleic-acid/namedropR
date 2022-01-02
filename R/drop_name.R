#' @title drop_name
#'
#' @description Extracts metadata from a .bib file and exports the visual citation in the specified format.
#'
#' @param file A .bib file.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param export_as A string specifying the desired output format. For now only supports HTML by using "html".
#' @param max_authors Integer number of maximum authors to print. If the number of authors exceeds this, the list is cropped accordingly.
#' @param include_qr Boolean value, whether to include a QR code (containing the URL to the DOI) next to the visual citation.
#' @param style A string specifying the desired style for the visual citation. Possible values are: "modern", "classic", "clean", "none". If "none" is given, the returned html can use a custom css file provided by the user. The custom CSS file must specify the DIV classes "top-row", "title-row" and "author-row".
#'
#' @return A visual citation
#'
#' @examples
#' \dontrun{
#' dropped <- drop_name(
#'   file = "sample_data/sample.bib",
#'   cite_key = NULL,
#'   export_as = "html",
#'   max_authors = 3
#' )
#'
#' htmltools::html_print(dropped)
#' }
#' @export
#' @importFrom bibtex "read.bib"
#' @importFrom htmltools div h1 h2 h3 img
#' @importFrom xfun base64_uri
#' @importFrom here here

drop_name <- function(bib_file = "sample_data/sample.bib", cite_key = "collaboration_2019_ApJL", output_dir = "visual_citations", export_as = "html", max_authors = 3, include_qr = TRUE, style = "modern") {


  # read the bibtex file
  bib <- bibtex::read.bib(file = bib_file)
  if (length(bib) == 0) {
    stop("BibTeX entry not found in the supplied file. Please check, that the citation key and the file are correct.")
  }

  # select the target reference entry by key
  if (cite_key %in% bib$key) {
    target_ref <- bib[cite_key]
  } else {
    stop("BibTeX entry not found in the supplied file. Please check, that the citation key and the file are correct.")
  }

  # obtain the actual number of authors to print (either the supplied max_author or less
  # if the actual number of authors is less)
  if (max_authors < length(target_ref$author)) {
    # if the author list is cropped, add "et. al."
    authors_collapsed <- paste0(
      paste(target_ref$author[1:max_authors], collapse = ", "),
      " et. al."
    )
  } else {
    authors_collapsed <- paste(target_ref$author, collapse = ", ")
  }

  if (!is.null(target_ref$doi)) {
    generate_qr_svg(url = paste0("https://doi.org/", target_ref$doi),
                    cite_key = target_ref$key)
  } else if (!is.null(target_ref$url)) {
    generate_qr_svg(url = target_ref$url,
                    cite_key = target_ref$key)
  } else {
    search_string <- paste0("https://scholar.google.com/scholar?as_q=", target_ref$author[1],
                            "+", target_ref$journal,
                            "+", target_ref$year)
    generate_qr_svg(url = search_string,
                    cite_key = target_ref$key)
  }

  vc_html <- drop_html(
    title = target_ref$title,
    journal = target_ref$journal,
    year = target_ref$year,
    authors = authors_collapsed,
    cite_key = target_ref$key,
    include_qr = include_qr,
    style = style
  )

  if(export_as == "html") {
    if (!dir.exists(here::here(output_dir))) {
      dir.create(here::here(output_dir))
    }
    # htmltools::html_print(vc_html)
    htmltools::save_html(vc_html, file = here::here(output_dir, paste0(target_ref$key, ".html")))
  } else if (export_as == "inline") {
    return(vc_html)
  } else {
    stop("Output format unknown")
  }

}
