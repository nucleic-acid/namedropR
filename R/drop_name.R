#' @title drop_name
#'
#' @description Extracts metadata from a .bib file and exports the visual citation in the specified format.
#'
#' @param bib Accepts one of the following:
#' A data.frame or tibble containing the columns YEAR, JOURNAL, AUTHOR, TITLE (all other columns will be dropped) or
#' a file path to a .bib file.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param export_as A string specifying the desired output format. For now supports PNG and HTML by
#' using "html" to include the 'bare' taglist or "html_full" to write a standalone .html file inlcuding <head> etc.
#' The PNG is a screenshot of the rendered HTML via the 'webshot' package. The filename represents this two step approach on purpose.
#' For webshot you need to install phantomJS once (see 'webshot' documentation).
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
#' @param use_xaringan Boolean to specify if an HTML output is intended to be included in an HTML presentation (like e.g. xaringan) or not.
#' When including the visual citation via htmltools::includeHTML(), the QR code needs to be in a subfolder
#' relative to the rendered presentation, not relative to the visual citation.
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
#' @import bib2df
#' @import dplyr
#' @importFrom htmltools tags save_html
#' @importFrom here here
#' @importFrom lubridate year ymd
#' @importFrom webshot webshot

drop_name <- function(bib, cite_key = "collaboration_2019_ApJL",
                      output_dir = "visual_citations",
                      export_as = "html",
                      inline = FALSE,
                      max_authors = 3,
                      include_qr = "link",
                      style = "modern",
                      substitute_missing = TRUE,
                      path_absolute = FALSE,
                      use_xaringan = FALSE) {

  # CHECK other ARGUMENTS
  stopifnot(is.character(output_dir))
  stopifnot(is.character(export_as))
  stopifnot(export_as %in% c("html", "html_full", "png"))
  stopifnot(is.logical(inline))
  stopifnot(is.numeric(max_authors))
  stopifnot(max_authors >= 0)
  stopifnot(is.character(include_qr))
  stopifnot(include_qr %in% c("embed", "link", "link_svg", "none"))
  stopifnot(is.character(style))
  # style content is not further checked, as unknown styles will be handled as "none" in get_css_styles()
  stopifnot(is.logical(substitute_missing))
  stopifnot(is.logical(path_absolute))
  stopifnot(is.logical(use_xaringan))

  # READ AND CHECK BIB FILE or BIBENTRY

  if (missing(bib)) {
    stop("No bibliography object or file path provided. Please check arguments.")
  }

  if (is.data.frame(bib)) {
    bib_data <- bib
  } else if (is.character(bib)) {
    if (file.exists(bib)) {
      bib_data <- bib2df::bib2df(file = bib)
      message("Bibliography file successfully read.")
    } else {
      stop("BibTeX file not found. Check file path or pass a suitable data.frame/tibble to the function.")
    }
  } else {
    stop("Inappropriate type bibliography provided, please pass a data.frame, tibble or file path to a *.bib file.")
  }

  # check if data.frame is empty
  if (length(bib_data) == 0) {
    stop("Bibliography is empty.")
  }

  # provide compatibility for bibtex and biblatex fields
  if ("date" %in% tolower(colnames(bib_data))) {
    message("One or more references had BibLaTeX field 'DATE' and were transformed to 'YEAR'.")
    bib_data <- bib_data %>%
      mutate(
        YEAR = ifelse(is.na(YEAR),
          tryCatch(
            expr = {
              lubridate::year(lubridate::ymd(DATE, truncated = 2))
            },
            error = function(e) {
              message("Could not extract Year from one of the references.")
              print(e)
            },
            warning = function(w) {
              message("Having difficulties extracting the Year from one of the refernces. Resulting output might not be as expected.")
              print(w)
            }
          ),
          YEAR
        )
      )
  }

  if ("JOURNALTITLE" %in% colnames(bib_data)) {
    message("One or more references had BibLaTeX field 'JOURNALTITLE' and were transformed to 'JOURNAL'.")
    bib_data <- bib_data %>%
      mutate(JOURNAL = ifelse(is.na(JOURNAL), JOURNALTITLE, JOURNAL))
  }

  # check for required columns
  required_cols <- c("YEAR", "AUTHOR", "TITLE", "JOURNAL", "BIBTEXKEY")
  if (!all(required_cols %in% colnames(bib_data))) {
    stop("Required data.frame columns are 'YEAR', 'AUTHOR', 'TITLE', 'JOURNAL', 'BIBTEXKEY' (all uppercase). At least one is missing or misspelled.")
  }

  # COMPOSE URL FOR QR CODE
  # If bib_data has a DOI column, use this as this first.
  # Next option is using the URL in the bib_data
  # If neither is available (or missing in some rows), a search query for Google Scholar will be passed to the QR code.
  # This will later be passed on to the HTML rendering function.

  if ("DOI" %in% colnames(bib_data)) {
    bib_data <- bib_data %>%
      mutate(
        QR = paste0("https://doi.org/", DOI)
      )
  }

  if ("URL" %in% colnames(bib_data)) {
    bib_data <- bib_data %>%
      mutate(
        QR = ifelse(
          is.na(QR),
          URL,
          QR
        )
      )
  }

  bib_data <- bib_data %>%
    mutate(
      QR = ifelse(
        is.na(QR),
        paste("https://scholar.google.com/scholar?as_q=", AUTHOR, JOURNAL, YEAR, collapse = "+"),
        QR
      )
    )


  # check for duplicate cite_keys
  if (any(dplyr::count(bib_data, BIBTEXKEY)$n > 1)) {
    warning("BIBTEX keys are not unique in this bibliography. Duplicates are dropped before proceding.")
  }

  # drop rows without BIBTEXKEY and select required columns only:
  clean_bib <- dplyr::distinct(bib_data, BIBTEXKEY, .keep_all = TRUE) %>%
    filter(!is.na(BIBTEXKEY)) %>%
    dplyr::select(YEAR, AUTHOR, JOURNAL, TITLE, BIBTEXKEY, QR)


  # create output directory, if needed
  if (!dir.exists(here::here(output_dir))) {
    tryCatch(
      expr = dir.create(here::here(output_dir)),
      error = function(e) {
        message("Could not create the output directory. Check file permissions.")
        print(e)
      },
      warning = function(w) {
        message("Having difficulties creating the output directory:")
        print(w)
      }
    )
  }

  # check if target reference entry is available | will be needed later on. uncommented for the time being.
  # if (cite_key %in% bib_data$BIBTEXKEY) {
  #   target_ref <- bib_file[cite_key]
  # } else {
  #   stop(paste0(cite_key, ": entry not found in the supplied bibliography. Please check, that the citation key and the bibliography are correct."))
  # }

  # CALL HTML RENDERING FUNCTION


  # handle different input options for bibtex keys
  if (missing(cite_key)) {
    n_bib <- nrow(clean_bib)
    message(paste0("No cite_key specified. Working through all ", n_bib, " entries in the bibliography."))
    work_list <- clean_bib
  } else if (!is.character(cite_key)) {
    stop("cite_key must be of type 'caracter'.")
  } else {
    n_bib <- length(cite_key)
    message(paste0(n_bib, " cite_key(s) specified. Working through all of them."))
    work_list <- clean_bib %>%
      filter(BIBTEXKEY %in% cite_key)
  }

  authors_collapsed <- sapply(
    work_list$AUTHOR, manage_authors, max_authors = max_authors
  )

  work_list <- work_list %>%
    mutate(
      authors_collapsed = authors_collapsed,
    )

  vcs = apply(
    work_list,
    1,
    drop_html2,
    include_qr = include_qr,
    style = style,
    output_dir = output_dir,
    # if PNG is desired output format, the relative filepath must not
    # be adapted for later inclusion of the HTML. Therefore use_xaringan
    # must be ignored, as otherwise the QR will not be included properly
    use_xaringan = ifelse(export_as == "png", FALSE, use_xaringan)
  )

  work_list <- work_list %>%
    mutate(
      vcs = vcs
    )

  if (inline) { # return the dataframe with the html tag list as result
    return(work_list)
  } else {
    file_paths <- apply(work_list, 1, FUN = write_vc,
          output_dir = output_dir,
          path_absolute = path_absolute,
          export_as = export_as
          )
  return(file_paths)
  }

}
