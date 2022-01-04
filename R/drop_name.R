#' @title drop_name
#'
#' @description Extracts metadata from a .bib file and exports the visual citation in the specified format.
#'
#' @param bib Accepts one of the following: A RefManageR BibEntry object or a file path to a .bib file.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param export_as A string specifying the desired output format. For now only supports HTML by
#' using "html" to include the 'bare' taglist or "html_full" to write a standalone .html file inlcuding <head> etc.
#' @param inline If TRUE, the output is directly returned. Otherwise the output is stored to disk and a filepath is returned as string.
#' See also 'path_absolute' parameter.
#' @param output_dir A string specifying the relative path, where the rendered output files should be stored.
#' @param max_authors Integer number of maximum authors to print. If the number of authors exceeds this, the list is cropped accordingly.
#' @param include_qr Character string specifying the way the QR code should be included or if no QR code should be included.
#' 'embed' results in a stand alone <img> tag within the HTML object, other options are ignored for the time being.
#' 'link' (default) creates a PNG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'link_svg' creates a SVG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'none' creates no QR code.
#' @param style A string specifying the desired style for the visual citation. Possible values are:
#' "modern", "classic", "clean", "none". If "none" is given, the returned html can use a custom css file provided by the user.
#' This custom CSS file must specify styles for <div> classes "top-row", "title-row" and "author-row".
#' @param substitute_missing Boolean to specify, whether missing data such as title, journal, year or authors should be substituted (TRUE, default) or left empty (FALSE).
#' @param path_absolute Boolean to specify, whether the returned output path (when inline = FALSE) is a relative path or an absolute path.
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
#' @importFrom RefManageR ReadBib
#' @importFrom htmltools tags save_html
#' @importFrom here here
#' @importFrom lubridate year ymd

drop_name <- function(bib, cite_key = "collaboration_2019_ApJL",
                      output_dir = "visual_citations",
                      export_as = "html",
                      inline = FALSE,
                      max_authors = 3,
                      include_qr = "link",
                      style = "modern",
                      substitute_missing = TRUE,
                      path_absolute = FALSE) {

  # CHECK other ARGUMENTS
  stopifnot(is.character(cite_key))
  stopifnot(is.character(output_dir))
  stopifnot(is.character(export_as))
  stopifnot(is.logical(inline))
  stopifnot(is.numeric(max_authors))
  stopifnot(is.character(include_qr))
  stopifnot(include_qr %in% c("embed", "link", "link_svg", "none"))
  stopifnot(is.character(style))
  stopifnot(is.logical(substitute_missing))

  # READ AND CHECK BIB FILE or BIBENTRY

  if (missing(bib)) {
    stop("No bibliography provided. Please check arguments.")
  }

  if (RefManageR::is.BibEntry(bib)) {
    bib_file <- bib
  } else if (is.character(bib)) {
    if (file.exists(bib)) {
      bib_file <- RefManageR::ReadBib(file = bib)
      message("Bibliography file successfully read.")
    } else {
      stop("BibTeX file not found. Check file path or pass a BibEntry object to the function.")
    }
  }

  # check if file is empty
  if (length(bib_file) == 0) {
    stop("Bibliography is empty.")
  }

  # select the target reference entry by key
  if (cite_key %in% bib_file$key) {
    target_ref <- bib_file[cite_key]
  } else {
    stop(paste0(cite_key, ": entry not found in the supplied bibliography. Please check, that the citation key and the bibliography are correct."))
  }

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

  # COMPOSE URL FOR QR CODE
  # If bibentry has a DOI number use this, as this is prob. the shortest URL possible.
  # Next option is using the provided URL in the bibentry.
  # If neither is abvailable, a search query for Google Scholar will be passed to the QR code.
  # This will later be passed on to the HTML rendering function.
  if (!is.null(target_ref$doi)) {
    url <- paste0("https://doi.org/", target_ref$doi)
  } else if (!is.null(target_ref$url)) {
    url <- target_ref$url
  } else {
    search_string <- paste0(
      "https://scholar.google.com/scholar?as_q=", target_ref$author[1],
      "+", target_ref$journal,
      "+", target_ref$year
    )
    url <- search_string
  }


  # CALL HTML RENDERING FUNCTION
  # create output directory, if needed
  if (!dir.exists(here::here(output_dir))) {
    dir.create(here::here(output_dir))
  }

  # provide backwards compatibility for bibtex
  if (is.null(target_ref$journal)) {
    target_ref$journal <- ifelse(is.null(target_ref$journaltitle),
      "",
      target_ref$journaltitle
    )
  }
  if (is.null(target_ref$year)) {
    target_ref$year <- ifelse(is.null(target_ref$date),
      "",
      lubridate::year(lubridate::ymd(target_ref$date, truncated = 2))
    )
  }

  # passes the completed data to the rendering function with specific options
  vc_html <- drop_html(
    title = target_ref$title,
    journal = target_ref$journal,
    year = target_ref$year,
    authors = authors_collapsed,
    url = url,
    cite_key = target_ref$key,
    include_qr = include_qr,
    style = style,
    output_dir = output_dir
  )

  # EXPORT RESULT
  # either returns the rendered object or the path to the stored file
  if (inline) {
    return(vc_html)
  } else {
    if (path_absolute) {
      output_path <- here::here(output_dir, paste0(target_ref$key, ".html"))
    } else {
      output_path <- file.path(output_dir, paste0(target_ref$key, ".html"))
    }

    if (export_as == "html_full") {
      htmltools::save_html(vc_html, file = here::here(output_path))
    } else if (export_as == "html") {
      write(as.character(vc_html), file = here::here(output_path))
    } else {
      stop("Output format unknown")
    }
    return(as.character(output_path))
  }
}
