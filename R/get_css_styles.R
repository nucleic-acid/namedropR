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
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#'
get_css_styles <- function(style) {

  # check for correct style argument class
  stopifnot(is.character(style))

  # read style table
  allowed_styles <- readr::read_csv(system.file("styles", "styles.csv", package = "namedropR"), show_col_types = FALSE)

  # print(allowed_styles)

  # initiate empty list
  css_styles <- list()

  if (style == "compact") {
    css_styles$compact_author_row <- paste("font-size: 2.2vw;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#000000;text-align: right;")
    css_styles$compact_year_row <- paste("font-size: 2.2vw;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: normal;color:#000000;text-align: right")

    # return the compact style list at this point
    return(css_styles)
  }

  if (style %in% allowed_styles$style_name) {
    allowed_styles <- dplyr::filter(allowed_styles, .data$style_name == style)
    # assign inline CSS strings to the respective list items according to the style argument
    css_styles$top_row_style <- allowed_styles$top_row
    css_styles$title_row_style <- allowed_styles$title_row
    css_styles$author_row_style <- allowed_styles$author_row
  } else if (style == "none") {
    css_styles$top_row_style <- ""
    css_styles$title_row_style <- ""
    css_styles$author_row_style <- ""
  } else {
    warning(paste0("This is not the style you are looking for...(*waves hand*). Provided CSS style '", style, "' is not defined. No CSS style is returned."))
    css_styles$top_row_style <- ""
    css_styles$title_row_style <- ""
    css_styles$author_row_style <- ""
  }

  # return the list
  return(css_styles)
}
