#' @title get_css_style
#'
#' @description Accepts bibliographic information and retuns a htmltools taglist for printing/display.
#'
#' @param style A string specifying the desired style for the visual citation.

#' @return A list of inline css styles for each element of the visual citation.
#'
#' @examples
#' \dontrun{
#' css_styles <- get_css_styles(style = "modern")
#' }
#'
get_css_styles <- function(style = "clean") {
  css_styles <- list()

  if (style == "modern") {
    css_styles$top_row_style <- paste("font-size: 1.3rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal;font-variant: small-caps;color:#ae2746;")
    css_styles$title_row_style <- paste("font-size: 2rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: bold")
    css_styles$author_row_style <- paste("font-size: 1.3rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal")
  }

  return(css_styles)
}
