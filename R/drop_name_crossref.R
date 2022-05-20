#' Create visual citation from Crossref data
#'
#' Takes one or several dois and extracts information from Crossref, then processes them into visual citations with \code{\link{drop_name}}
#'
#' @param dois One or several dois to create visual citations for
#' @inheritDotParams drop_name -bib -cite_key
#'
#' @export

drop_name_crossref <- function(dois, ...) {

  if (!require(rcrossref)) stop("This function requires the rcrossref package. Please install and retry.")
  stopifnot(is.character(dois))

  ref <-  rcrossref::cr_cn(dois, "citeproc-json")

  to_df <- function(x) {
    authors <- dplyr::pull(dplyr::mutate(x$author, name = paste0(family, ", ", given)))
    author_last <- x$author$family[1]
    dplyr::tibble(YEAR = x$created$`date-parts`[1], JOURNAL = x$`container-title`, AUTHOR = list(authors),
                  TITLE = x$title, BIBTEXKEY = paste(author_last, x$created$`date-parts`[1]), DOI = x$DOI)
  }

  df <- dplyr::bind_rows(lapply(ref, to_df))

  #Ensure BIBTEXKEY are unique
  letters_blank <- c("", letters)
  df <- dplyr::ungroup(dplyr::mutate(dplyr::group_by(df, BIBTEXKEY), BIBTEXKEY = paste0(BIBTEXKEY, letters_blank[1:dplyr::n()])))

  drop_name(df, ...)

}
