#' @title check_bib_complete
#'
#' @description Checks for missing essential bibliographic data and optionally substitutes with dummy data.
#' Always throws a warning with reference to the bibentry.
#' Substitutions are inspired by the APA style guide
#' (https://apastyle.apa.org/style-grammar-guidelines/references/missing-information)
#'
#' @param substitute_missing Boolean to specify, whether missing data such as title, journal, year or authors should be substituted (TRUE, default) or left empty (FALSE).
#' @param ref_item A bibentry item containing the data
#'
#' @return A complete bibentry object. Complete means all necessary fields (author, year, journal, title) are available, but may be empty strings.
#'
#' @examples
#' \dontrun{
#' check_bib_complete(substitute_missing = FALSE, ref_item = target_ref)
#' }
#'
check_bib_complete <- function(substitute_missing, ref_item) {

  # CHECK INPUTS
  stopifnot(class(substitute_missing) == "logical")
  stopifnot(class(ref_item) == "bibentry")


  # substitute all NULL elements with somewhat meaningful replacements.
  if (substitute_missing) { # if substitute_missing is TRUE
    if (is.null(ref_item$title)) {
      warning(paste0(ref_item$key, ": No Title supplied. Will be substituted with 'No Title'"))
      ref_item$title <- "No Title"
    }
    if (is.null(ref_item$journal)) {
      warning(paste0(ref_item$key, ": No Journal Name supplied. Will be substituted with 'Unknown Journal'"))
      ref_item$journal <- "Unknown Journal"
    }
    if (is.null(ref_item$year)) {
      warning(paste0(ref_item$key, ": No publication year supplied. Will be substituted with 'n.d.'"))
      ref_item$year <- "n.d."
    }
    if (is.null(ref_item$author)) {
      warning(paste0(ref_item$key, ": No author(s) supplied. Will be substituted with 'Unkown Author'"))
      ref_item$author <- "Unknown Author"
    }
  } else { # if substitute_missing is FALSE
    if (is.null(ref_item$title)) {
      warning(paste0(ref_item$key, ": No Title supplied. Will be left empty"))
      ref_item$title <- ""
    }
    if (is.null(ref_item$journal)) {
      warning(paste0(ref_item$key, ": No Journal Name supplied. Will be left empty"))
      ref_item$journal <- ""
    }
    if (is.null(ref_item$year)) {
      warning(paste0(ref_item$key, ": No publication year supplied. Will be left empty"))
      ref_item$year <- ""
    }
    if (is.null(ref_item$author)) {
      warning(paste0(ref_item$key, ": No author(s) supplied. Will be left empty"))
      ref_item$author <- ""
    }
  }

  return(ref_item)
}
