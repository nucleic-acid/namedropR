#' @title get_css_style
#'
#' @description Provides inline CSS code for three distinct visual citation styles.
#' If "none" is given, the returned styles are empty strings.
#'
#' @param style A string specifying the desired style for the visual citation.

#' @return A list of inline css styles for each element of the visual citation: top row, title row and author row.
#'
#' @examples
#' modern_style <- get_css_styles(style = "modern")
#' print(modern_style$title_row_style)
#'

get_css_styles <- function(style) {
  css_styles <- list()

  if (style == "classic") {
    css_styles$top_row_style <- paste("font-size: 1.3rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal;font-variant: small-caps;color:#e72e00;")
    css_styles$title_row_style <- paste("font-size: 2rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: bold")
    css_styles$author_row_style <- paste("font-size: 1.1rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal")
  } else if (style == "modern") {
    css_styles$top_row_style <- paste("font-size: 1.2rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: lighter;text-transform: uppercase;")
    css_styles$title_row_style <- paste("font-size: 2rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#1A3399;")
    css_styles$author_row_style <- paste("font-size: 1.1rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#479BC5;")
  } else if (style == "clean") {
    css_styles$top_row_style <- paste("font-size: 1rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#479BC5;")
    css_styles$title_row_style <- paste("font-size: 2rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: lighter;text-transform: uppercase;")
    css_styles$author_row_style <- paste("font-size: 1.5rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;")
  }

  return(css_styles)
}
