#' @title manage_authors
#'
#' @description Returns a cleaned and cropped string of authors for the visual citation.
#'
#' @param authors Can be a single string or a list of authors.
#' @param max_authors Maximum number of authors to be returned from the list.
#' @param style Takes the user specified style. Only relevant for "compact" mode. Ignored otherwise.
#'
#' @return A single string with the desired maximum number of authors.
#'
manage_authors <- function(authors, max_authors, style = "any") {
  stopifnot(any(is.character(authors), is.list(authors)))
  stopifnot(is.numeric(max_authors))

  # To make the function "understand" both lists and vectors, both options are accepted.
  # if a list of authors is passed, flatten the list (unlist),
  # then combine the authors to a single string
  if (is.list(authors)) {
    authors <- unlist(authors)
  }

  if (style == "compact") {
    max_authors <- 1
  }

  # if many authors are passed, crop the list according to option max_authors
  if (max_authors < length(authors)) {

    # if the author list is cropped, add "et. al."
    # in "compact" style, the et. al. goes to a new line
    if (style == "compact") {
      authors <- paste0(
        sub(",.*", "", authors[1:max_authors]),
        "<br>et. al."
      )
    } else {
      authors <- paste0(
        paste(authors[1:max_authors],
          collapse = "; "
        ),
        " et. al."
      )
    }
  } else {
    # if there are less authors than the desired maximum, just paste them together
    authors <- paste(authors[1:length(authors)], collapse = "; ")
  }

  # curly braces are often present in bibtex entries
  # removing them from the authors' names is probably save
  # as no technical necessity is expected within the scope of this package.
  authors_cleaned <- gsub("\\{|\\}", "", authors)
  return(authors_cleaned)
}
