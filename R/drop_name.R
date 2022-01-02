#' @title drop_name
#'
#' @description Extracts metadata from a .bib file and exports the visual citation in the specified format.
#'
#' @param file A .bib file.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param export_as A string specifying the desired output format. For now only supports HTML by using "html".
#' @param max_authors Integer number of maximum authors to print. If the number of authors exceeds this, the list is cropped accordingly.
#'
#' @return A visual citation
#'
#' @examples
#' \dontrun{
#' dropped <- drop_name(
#'  file = "sample_data/sample.bib",
#'  cite_key = NULL,
#'  export_as = "html",
#'  max_authors = 3)
#'
#'  htmltools::html_print(dropped)
#'  }
#' @export
#' @importFrom bibtex "read.bib"
#' @importFrom htmltools div h1 h2 h3
#' @import ggplot2

drop_name <- function(file = "sample_data/sample.bib", cite_key = NULL, export_as = "html", max_authors = 3) {

  # define required packages
  # require(bibtex)
  # require(ggplot2)
  # requireNamespace(htmltools)

  # read the bibtex file
  bib <- bibtex::read.bib(file = file)
  if (length(bib) == 0) {
    stop("No BibTeX entry in the supplied file.")
  }

  # select the target reference entry by key
  if (is.null(cite_key)) {
    target_ref <- bib[1]
  } else {
    target_ref <- bib[bib$key == cite_key]
  }

  print(target_ref$doi)

  # obtain the actual number of authors to print (either the supplied max_author or less
  # if the actual number of authors is less)
  if (max_authors < length(target_ref$author)) {
    # if the author list is cropped, add "et. al."
    authors_list <- paste0(
      paste(target_ref$author[1:max_authors], collapse = ", "),
      " et. al."
    )
  } else {
    authors_list <- paste(paste(target_ref$author, collapse = ", "))
  }


  # ref_title = target_ref$title

  vc <- htmltools::div(
    class = "visual-citation",
    htmltools::div(
      class = "top-row",
      htmltools::h2(paste0(target_ref$journal, " (", target_ref$year, ")"))
    ),
    htmltools::div(
      class = "title-row",
      htmltools::h1(target_ref$title)
    ),
    htmltools::div(
      class = "author-row",
      htmltools::h3(authors_list),
    )
  )


  return(vc)

}

