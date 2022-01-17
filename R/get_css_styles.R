#' @title get_css_style
#'
#' @description Provides inline CSS code for three distinct visual citation styles.
#' If "none" is given, the returned styles are empty strings.
#'
#' @param style A string specifying the desired style for the visual citation. Possible values are:
#' "modern", "classic", "clean", "none". If "none" is given, the returned html can use a custom css file provided by the user.
#' This custom CSS file must specify styles for <div> classes "top-row", "title-row" and "author-row".
#'
#' @return A list of inline css styles for each element of the visual citation: top row, title row and author row.
#'
#'
get_css_styles <- function(style) {

  # check for correct style argument
  allowed_styles <- c("classic", "modern", "clean", "compact", "none")

  if (!style %in% allowed_styles) {
    warning(paste0("Provided CSS style '", style, "' is not defined. No CSS code is returned."))
    style <- "none"
  }

  # initiate empty list
  css_styles <- list()

  # assign inline CSS strings to the respective list items according to the style argument
  if (style == "classic") {
    css_styles$top_row_style <- paste("font-size: 1.3rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal;font-variant: small-caps;color:#e72e00;")
    css_styles$title_row_style <- paste("font-size: 2rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: bold")
    css_styles$author_row_style <- paste("font-size: 1.1rem;font-family: 'Palatino', 'Georgia', 'Times New Roman', serif;font-weight: normal")
  } else if (style == "modern") {
    css_styles$top_row_style <- paste("font-size: 1.2rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: lighter;")
    css_styles$title_row_style <- paste("font-size: 2rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#1A3399;")
    css_styles$author_row_style <- paste("font-size: 1.1rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#479BC5;")
  } else if (style == "clean") {
    css_styles$top_row_style <- paste("font-size: 1rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#479BC5;")
    css_styles$title_row_style <- paste("font-size: 2rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: lighter;text-transform: uppercase;")
    css_styles$author_row_style <- paste("font-size: 1.5rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;")
  } else if (style == "none") {
    css_styles$top_row_style <- ""
    css_styles$title_row_style <- ""
    css_styles$author_row_style <- ""
  } else if (style == "compact") {
    css_styles$compact_author_row <- paste("font-size: 1.3rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#000000;text-align: right;")
    css_styles$compact_year_row <- paste("font-size: 1.3rem;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: normal;color:#000000;text-align: right")
  } else {
    stop("Something unexpected happend resolving the CSS style. Check the function call and the supplied style argument.")
  }



  # return the list
  return(css_styles)
}
