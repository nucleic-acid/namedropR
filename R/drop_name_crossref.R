#' Create visual citation from Crossref data
#'
#' Takes one or several dois and extracts information from Crossref, then processes them into visual citations with \code{\link{drop_name}}. Requires {rcrossref} > v1.1, see README for further details.
#'
#' @param dois One or several dois to create visual citations for. If they are named, these names are used as filenames; otherwise they are generated based on authors and years.
#' @inheritDotParams drop_name -bib -cite_key
#'
#' @return A character string with the file path to the created visual citation in the specified output format.
#'
#'@examples
#' \dontrun{
#' drop_name_crossref(c(cite1 = "10.1126/science.169.3946.635", cite2 = "10.1111/joms.12670"))
#' }
#'
#' @export

drop_name_crossref <- function(dois, ...) {

  if (!"rcrossref" %in% rownames(utils::installed.packages())) {
    stop("This function requires the rcrossref package. Please install and retry.")
  }

  stopifnot(is.character(dois))

  ref <-  tryCatch(rcrossref::cr_cn(dois, "citeproc-json"),
    error=function(cond) {
      message('rcrossref failed to retrieve the requested data.\n
      You might need to provide your email address to crossref: \n run file.edit("~/.Renviron"), then add crossref_email= "name@example.com"
      You might also want to check the status of the crossref API on http://status.crossref.org')
      message("\nHere's the original error message:")
      stop(cond, call. = FALSE)
    })


  to_df <- function(x) {
    authors <- dplyr::pull(dplyr::mutate(x$author, name = paste0(x$author$family, ", ", x$author$given)))
    author_last <- x$author$family[1]
    dplyr::tibble(YEAR = x$created$`date-parts`[1], JOURNAL = x$`container-title`, AUTHOR = list(authors),
                  TITLE = x$title, `BIBTEXKEY` = paste(author_last, x$created$`date-parts`[1]), DOI = x$DOI)
  }

  if (length(dois) == 1) {
    ref <- list(ref)
  }

  ref <- ref[!sapply(ref, is.null)] #NULLs result from invalid DOIs

  if (length(ref) == 0) stop("No results found - check your DOIs or crossref connection")

  df <- dplyr::bind_rows(lapply(ref, to_df))

  #Use names of DOIs as keys where provided
  if(!is.null(names(dois))) {
    df$BIBTEXKEY[stringr::str_length(names(dois))>0] <- names(dois)[stringr::str_length(names(dois))>0]
  }

  drop_name(df, ...)

}
