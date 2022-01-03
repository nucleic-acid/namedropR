#' @title drop_name
#'
#' @description Extracts metadata from a .bib file and exports the visual citation in the specified format.
#'
#' @param bib_file A BibTeX file *.bib.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param export_as A string specifying the desired output format. For now only supports HTML by using "html".
#' @param inline If TRUE, the output is directly returned. Otherwise the output is stored to disk and a filepath is returned as string.
#' @param output_dir A string specifying the relative path, where the rendered output files should be stored.
#' @param max_authors Integer number of maximum authors to print. If the number of authors exceeds this, the list is cropped accordingly.
#' @param include_qr Boolean value, whether to include a QR code (containing the URL to the DOI) next to the visual citation.
#' @param style A string specifying the desired style for the visual citation. Possible values are:
#' "modern", "classic", "clean", "none". If "none" is given, the returned html can use a custom css file provided by the user.
#' This custom CSS file must specify styles for <div> classes "top-row", "title-row" and "author-row".
#' @param substitute_missing Boolean to specify, whether missing data such as title, journal, year or authors should be substituted (TRUE, default) or left empty (FALSE).
#'
#' @return A visual citation in the specified output format.
#'
#' @examples
#' \dontrun{
#' dropped <- drop_name(
#'   file = system.file("testdata", "sample.bib", package = "namedropR"),
#'   cite_key = "collaboration_2019_ApJL",
#'   export_as = "html",
#'   inline = TRUE,
#'   max_authors = 3
#' )
#'
#' htmltools::html_print(dropped)
#' }
#' @export
#' @importFrom bibtex "read.bib"
#' @importFrom htmltools tags save_html
#' @import xfun
#' @import mime
#' @importFrom here here

drop_name <- function(bib_file = "inst/testdata/sample.bib", cite_key = "collaboration_2019_ApJL",
                      output_dir = "visual_citations", export_as = "html", inline = TRUE,
                      max_authors = 3,
                      include_qr = TRUE, style = "modern",
                      substitute_missing = TRUE) {

  # CHECK ARGUMENTS
  stopifnot(class(bib_file) == "character")
  stopifnot(class(cite_key) == "character")
  stopifnot(class(output_dir) == "character")
  stopifnot(class(export_as) == "character")
  stopifnot(class(inline) == "logical")
  stopifnot(class(max_authors) == "numeric")
  stopifnot(class(include_qr) == "logical")
  stopifnot(class(style) == "character")
  stopifnot(class(substitute_missing) == "logical")

  # READ AND CHECK BIB FILE AND TARGET BIB-ENTRY
  # read the bibtex file if it exists
  if (file.exists(bib_file)) {
    bib <- bibtex::read.bib(file = bib_file)
  } else {
    stop("BibTeX file not found. Check file path.")
  }

  # check if file is empty
  if (length(bib) == 0) {
    stop("BibTeX file is empty.")
  }

  # select the target reference entry by key
  if (cite_key %in% bib$key) {
    target_ref <- bib[cite_key]
  } else {
    stop("BibTeX entry not found in the supplied file. Please check, that the citation key and the file are correct.")
  }


  # CHECK COMPLETENESS OF DATA
  # reassign to target_ref after completing
  target_ref <- check_bib_complete(
    substitute_missing = substitute_missing,
    ref_item = target_ref
  )

  # COLLAPSE AUTHOR LIST
  # Obtain the actual number of authors to print
  # (either the supplied max_author or less if the actual number of authors is less).
  # Collapse the authors list to a single comma separated string in the process:
  if (max_authors < length(target_ref$author)) {

    # if the author list is cropped, add "et. al."
    authors_collapsed <- paste0(
      paste(target_ref$author[1:max_authors], collapse = ", "),
      " et. al."
    )
  } else {
    authors_collapsed <- paste(target_ref$author, collapse = ", ")
  }

  # COMPOSE URL FOR QR CODE and PASS TO QR-GENERATION
  # If bibentry has a DOI number use this, as this is prob. the shortest URL possible.
  # Next option is using the provided URL in the bibentry.
  # If neither is abvailable, a search query for Google Scholar will be passed to the QR code.
  if (!is.null(target_ref$doi)) {
    generate_qr_svg(
      url = paste0("https://doi.org/", target_ref$doi),
      cite_key = target_ref$key
    )
  } else if (!is.null(target_ref$url)) {
    generate_qr_svg(
      url = target_ref$url,
      cite_key = target_ref$key
    )
  } else {
    search_string <- paste0(
      "https://scholar.google.com/scholar?as_q=", target_ref$author[1],
      "+", target_ref$journal,
      "+", target_ref$year
    )
    generate_qr_svg(
      url = search_string,
      cite_key = target_ref$key
    )
  }


  # CALL HTML RENDERING FUNCTION
  # passes the completed data to the rendering function with specific options
  vc_html <- drop_html(
    title = target_ref$title,
    journal = target_ref$journal,
    year = target_ref$year,
    authors = authors_collapsed,
    cite_key = target_ref$key,
    include_qr = include_qr,
    style = style
  )

  # EXPORT RESULT
  # either returns the rendered object or the path to the stored file
  if (export_as == "html") {
    if (inline) {
      return(vc_html)
    } else {
      if (!dir.exists(here::here(output_dir))) {
        dir.create(here::here(output_dir))
      }
      htmltools::save_html(vc_html, file = here::here(output_dir, paste0(target_ref$key, ".html")))
      return(as.character(here::here(output_dir, paste0(target_ref$key, ".html"))))
    }
  } else {
    stop("Output format unknown")
  }
}
