#' @title drop_name
#'
#' @description Extracts metadata from a .bib file and exports the visual citation in the specified format.
#'
#' @param bib Accepts one of the following:
#' 1) A data.frame or tibble containing the columns YEAR, JOURNAL, AUTHOR, TITLE, BIBTEXKEY (all mandatory) and DOI, URL (optional).
#' 2) A file path to a bibliography file in BibTeX/BibLaTeX format (usually *.bib file).
#' @param cite_key If given, either a character string or a vector of strings are accepted.
#' Specifies the reference items within the bibliography for which visual citations should be created.
#' If no key is specified, a visual citation is created for ALL reference items within the bibliography.
#' In other words, either one, many or no BibTeX citation keys can be specified.
#' @param export_as A string specifying the desired output format. For now supports PNG and HTML.
#' Use "html" to include the 'bare' taglist (recommended for inclusion in Rmarkdown documents) or "html_full" to write a standalone .html file including <head> etc.
#' The PNG is a screenshot of the rendered HTML via the 'webshot' package. The filename represents this two step approach on purpose.
#' For webshot you need to install phantomJS once (see 'webshot' documentation).
#' @param output_dir A string specifying the relative path, where the rendered output files should be stored.
#' @param max_authors Integer number of maximum authors to print. If the number of authors exceeds this, the list is cropped accordingly.
#' @param include_qr Character string specifying the way the QR code should be included or if no QR code should be included.
#' 'embed' results in a stand alone <img> tag within the HTML object, other options are ignored for the time being.
#' 'link' (default) creates a PNG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'link_svg' creates a SVG of the QR code and stores it in a subfolder of the HTML file's location. The HTML <img> tag links to this file then.
#' 'none' creates no QR code.
#' @param style A string specifying the desired style for the visual citation. Possible values are:
#' "modern", "classic", "clean", "fancy", "newspaper", "compact" and "none".
#' If "compact" is given, the rendered VC contains
#' only the last name of the first author and the publication year, next to the QR code.
#' If "none" is given, the returned html can use a custom css file provided by the user.
#' This custom CSS file must specify styles for <div> classes "top-row", "title-row" and "author-row". (see vignette)
#' @param path_absolute Boolean to specify, whether the returned output path is a relative path or an absolute path.
#' @param use_xaringan Boolean to specify if an HTML output is intended to be included in an HTML presentation (like e.g. xaringan) or not.
#' When including the visual citation via htmltools::includeHTML(), the QR code needs to be in a subfolder
#' relative to the rendered presentation, not relative to the visual citation.
#' @param clean_strings Removes curly braces {} from titles and journal names, as they are often present in
#' BibTeX strings, but not needed for the rendering. TRUE by default, but can be set to FALSE, if the {} are needed.
#' @param qr_size Specifies the height/width of the rendered QR code in px. Default: 250px, minimum: 150px. Ignored for SVG output.
#' @param vc_width Specifies the width of the text part of the visual citation in px.
#' This can be adjusted to accommodate e.g. untypically long or short titles. Default: 600px
#' @return A character string with the file path to the created visual citation in the specified output format.
#'
#' @examples
#'
#' # create sample data
#' \dontrun{
#' bib_tbl <- dplyr::tribble(
#'   ~TITLE, ~AUTHOR, ~JOURNAL, ~BIBTEXKEY, ~YEAR,
#'   "Some title", c("Alice", "Bob", "Charlie"),
#'   "Journal of Unnecessary R Packages",
#'   "Alice2022", "2022"
#' )
#'
#' # create visual citation
#' drop_name(
#'   bib = bib_tbl,
#'   cite_key = "Alice2022",
#'   export_as = "png",
#'   max_authors = 2,
#'   style = "clean",
#'   output_dir = tempdir()
#' )
#' }
#'
#' @export
#' @import bib2df
#' @import dplyr
#' @importFrom htmltools tags save_html
#' @importFrom here here
#' @importFrom lubridate year ymd
#' @importFrom webshot webshot

drop_name <- function(bib, cite_key,
                      output_dir = "visual_citations",
                      export_as = "html",
                      max_authors = 3,
                      include_qr = "link",
                      qr_size = 250,
                      vc_width = 600,
                      style = "modern",
                      path_absolute = FALSE,
                      use_xaringan = FALSE,
                      clean_strings = TRUE) {

  # CHECK other ARGUMENTS
  stopifnot(is.character(output_dir))
  stopifnot(is.character(export_as))
  stopifnot(export_as %in% c("html", "html_full", "png"))
  stopifnot(is.numeric(max_authors))
  stopifnot(max_authors >= 0)
  stopifnot(is.character(include_qr))
  stopifnot(include_qr %in% c("embed", "link", "link_svg", "none"))
  stopifnot(is.numeric(qr_size))
  if (qr_size < 150) {
    warning("QR size must be at least 150px. This will now be set as size.")
    qr_size <- 150
  }
  stopifnot(vc_width > 0)
  if (vc_width < 200) {
    message("You specified a fairly small 'vc_width'. If you want to have a compact visual citation, the 'compact' style might be handy.")
  }
  stopifnot(is.character(style))
  # style content is not further checked, as unknown styles will be handled as "none" in get_css_styles()
  stopifnot(is.logical(path_absolute))
  stopifnot(is.logical(use_xaringan))
  stopifnot(is.logical(clean_strings))

  # READ AND CHECK BIB FILE or BIBENTRY

  if (missing(bib)) {
    stop("No bibliography object or file path provided. Please check arguments.")
  }

  if (is.data.frame(bib)) {
    bib_data <- bib
  } else if (is.character(bib)) {
    if (file.exists(bib)) {
      bib_data <- suppressMessages(suppressWarnings(bib2df::bib2df(file = bib)))
      if ("YEAR" %in% colnames(bib_data)) {
        if (!is.character(bib_data$YEAR)) {
          bib_data$YEAR <- as.character(bib_data$YEAR)
          message("Years coerced to string format.")
        }
      }
      message("Bibliography file successfully read.")
    } else {
      stop("BibTeX file not found. Check file path or pass a suitable data.frame/tibble to the function.")
    }
  } else {
    stop("Inappropriate type of bibliography provided, please pass a data.frame, tibble or file path to a *.bib file.")
  }

  # check if data.frame is empty
  if (nrow(bib_data) == 0) {
    stop("Bibliography is empty.")
  }

  # provide COMPATIBILITY for biblatex fields JOURNALTITLE and DATE
  if ("DATE" %in% colnames(bib_data)) {
    parsed_year <- suppressWarnings(lubridate::year(lubridate::ymd(bib_data$DATE, truncated = 2)))
    # if (any(is.na(parsed_year))) {
    #   warning("Having difficulties extracting the year from at least one of the references' date field. Resulting output might not be as expected. Please make sure your dates comply with BibLaTeX standards 'YYYY', 'YYYY-MM' or 'YYYY-MM-DD'.")
    # }
    if ("YEAR" %in% colnames(bib_data)) {
      bib_data <- bib_data %>%
        dplyr::mutate(
          YEAR = ifelse(
            is.na(.data$YEAR),
            parsed_year,
            .data$YEAR
          )
        )
    } else {
      bib_data$YEAR <- parsed_year
    }
    # At this point no further error catching is implemented. As of current knowledge lubridate::year()
    # returns NA, if parsing the string fails. In drop_html() these NAs will be replaced with an empty string.
    # message("One or more references had BibLaTeX field 'DATE' and were transformed to 'YEAR'.")
  }


  if ("JOURNALTITLE" %in% colnames(bib_data)) {
    if ("JOURNAL" %in% colnames(bib_data)) {
      bib_data <- bib_data %>%
        dplyr::mutate(JOURNAL = ifelse(is.na(.data$JOURNAL), .data$JOURNALTITLE, .data$JOURNAL))
    } else {
      bib_data <- bib_data %>%
        dplyr::rename(JOURNAL = .data$JOURNALTITLE)
    }
    # message("One or more references had BibLaTeX field 'JOURNALTITLE' and were transformed to 'JOURNAL'.")
  }

  # print(bib_data)

  # check for required columns
  required_cols <- c("YEAR", "AUTHOR", "TITLE", "JOURNAL", "BIBTEXKEY")
  if (!all(required_cols %in% colnames(bib_data))) {
    stop("Required data.frame columns are 'YEAR' (or 'DATE'), 'AUTHOR', 'TITLE', 'JOURNAL', 'BIBTEXKEY' (all uppercase). At least one is missing or misspelled.")
  }

  # CLEAN STRINGS

  if (clean_strings) {
    bib_data <- bib_data %>%
      dplyr::mutate(
        TITLE = gsub("\\{|\\}", "", .data$TITLE),
        JOURNAL = gsub("\\{|\\}", "", .data$JOURNAL)
      )
  }

  # COMPOSE URL FOR QR CODE
  # If bib_data has a DOI column, use this as this first.
  # Next option is using the URL in the bib_data
  # If neither is available (or missing in some rows), a search query for Google Scholar will be passed to the QR code.
  # This will later be passed on to the HTML rendering function.

  # initialize empty QR column
  bib_data$QR <- NA

  # if DOI is available adapt this to the QR column
  if ("DOI" %in% colnames(bib_data)) {
    bib_data <- bib_data %>%
      dplyr::mutate(
        QR = ifelse(
          is.na(.data$DOI),
          NA,
          paste0("https://doi.org/", .data$DOI)
        )
      )
  }

  # if NO DOI but a URL is available adapt this to the QR column
  if ("URL" %in% colnames(bib_data)) {
    bib_data <- bib_data %>%
      dplyr::mutate(
        QR = ifelse(
          is.na(.data$QR) & !is.na(.data$URL),
          .data$URL,
          .data$QR
        )
      )
  }

  # the remaining URLs will be created below, after condensing the authors lists


  # check for duplicate cite_keys
  if (any(dplyr::count(bib_data, .data$BIBTEXKEY)$n > 1)) {
    warning("BIBTEX keys are not unique in this bibliography. Duplicates are dropped before proceding.")
  }

  # drop rows without BIBTEXKEY and select required columns only:
  clean_bib <- dplyr::distinct(bib_data, .data$BIBTEXKEY, .keep_all = TRUE) %>%
    dplyr::filter(!is.na(.data$BIBTEXKEY)) %>%
    dplyr::select(.data$YEAR, .data$AUTHOR, .data$JOURNAL, .data$TITLE, .data$BIBTEXKEY, .data$QR)


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


  # CALL HTML RENDERING FUNCTION


  # handle different input options for bibtex keys
  if (missing(cite_key)) {
    n_bib <- nrow(clean_bib)
    message(paste0("No cite_key specified. Working through all possible ", n_bib, " entries in the bibliography."))
    work_list <- clean_bib
  } else if (!is.character(cite_key)) {
    stop("cite_key must be of type 'caracter'.")
  } else {
    n_bib <- length(cite_key)
    if (n_bib > 1) {
      message(paste0(n_bib, " cite_key(s) specified. Working through all of them."))
    }
    missing_keys <- cite_key[!(cite_key %in% clean_bib$BIBTEXKEY)]
    if (length(missing_keys) > 0) {
      warning(paste0("The following cite_key items were not found in the provided library: ", missing_keys))
    }
    work_list <- clean_bib %>%
      dplyr::filter(.data$BIBTEXKEY %in% cite_key)
  }

  if (nrow(work_list) == 0) {
    warning("No reference matches the given cite_keys. Please check that citation key(s) are correct.")
    return("No Visual Citation created.")
  }

  authors_collapsed <- sapply(
    work_list$AUTHOR, manage_authors,
    max_authors = max_authors,
    style = style
  )


  work_list <- work_list %>%
    dplyr::mutate(
      authors_collapsed = authors_collapsed,
    )

  # now create QR URLs for remaining missing entries with short author list
  work_list <- work_list %>%
    dplyr::mutate(
      QR = ifelse(
        is.na(.data$QR),
        paste0(
          "https://scholar.google.com/scholar?as_q=",
          sub("[];<].*", "", .data$authors_collapsed), "+",
          .data$JOURNAL, "+",
          .data$YEAR
        ),
        .data$QR
      )
    )

  vcs <- apply(
    work_list,
    1,
    drop_html,
    include_qr = include_qr,
    style = style,
    output_dir = output_dir,
    # if PNG is desired output format, the relative filepath must not
    # be adapted for later inclusion of the HTML. Therefore use_xaringan
    # must be ignored, as otherwise the QR will not be included properly
    use_xaringan = ifelse(export_as == "png", FALSE, use_xaringan),
    qr_size = qr_size,
    vc_width = vc_width
  )

  work_list <- work_list %>%
    dplyr::mutate(
      vcs = vcs
    )

  file_paths <- apply(work_list, 1,
    FUN = write_vc,
    output_dir = output_dir,
    path_absolute = path_absolute,
    export_as = export_as
  )
  return(file_paths)
}
