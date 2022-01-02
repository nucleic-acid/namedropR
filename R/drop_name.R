#' @title drop_name
#'
#' @description Extracts metadata from a .bib file and exports the visual citation in the specified format.
#'
#' @param file A .bib file.
#' @param cite_key A string specifying the citation key within the .bib file. If no key is specified, the first entry is used.
#' @param export_as A string specifying the desired output format. For now only supports PNG.
#'
#' @return A visual citation
#'
#' @export
#' @importFrom bibtex "read.bib"
#' @importFrom png "png"
#' @import ggplot2

drop_name <- function(file = "data/sample.bib", cite_key = NULL, export_as = "html", max_authors = 3) {

  # define required packages
  require(bibtex)
  require(png)
  require(htmltools)

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

  vc <- div(
    class = "visual-citation",
    div(
      class = "top-row",
      h2(paste0(target_ref$journal, " (", target_ref$year, ")"))
    ),
    div(
      class = "title-row",
      h1(target_ref$title)
    ),
    div(
      class = "author-row",
      h3(authors_list),
    )
  )

  vc

  html_print(vc)

}

