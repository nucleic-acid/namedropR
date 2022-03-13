#' @title get_css_style
#'
#' @description Provides inline CSS code for three distinct visual citation styles.
#' If "none" is given, the returned styles are empty strings.
#'
#' @param style A string specifying the desired style for the visual citation. Possible values are:
#' "modern", "classic", "clean", "none". If "none" is given, the returned html can use a custom css file provided by the user.
#' This custom CSS file must specify styles for <div> classes "top-row", "title-row" and "author-row".
#' @param custom_style Style arguments passed by drop_html(). Can be specified in function call of drop_name().
#'
#' @return A list of inline css styles for each element of the visual citation: top row, title row and author row.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter
#' @importFrom stringr str_replace
#'
get_css_styles <- function(style, custom_style = list()) {

  # check for correct style argument class
  stopifnot(is.character(style))

  # check for correct custom style argument classes
  stopifnot(is.list(custom_style))
  for (style_item in custom_style) {
    stopifnot(is.character(style_item))
  }

  # read style table
  allowed_styles <- readr::read_csv(system.file("styles", "styles.csv", package = "namedropR"), show_col_types = FALSE)

  # print(allowed_styles)
  # print(custom_style)

  # initiate empty list
  css_styles <- list()

  if (style == "compact") {
    css_styles$compact_author_row <- paste("font-size: 2.2vw;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: bold;color:#000000;text-align: right;")
    css_styles$compact_year_row <- paste("font-size: 2.2vw;font-family: 'Noto Sans', 'Arial', 'Helvetica', sans-serif;font-weight: normal;color:#000000;text-align: right")

    # return the compact style list at this point
    return(css_styles)
  }


  if (style %in% allowed_styles$style_name) {
    base_style <- dplyr::filter(allowed_styles, .data$style_name == style)

    # swap style's predefined values with individual adjustments
    ## TOP ROW
    if (!is.null(custom_style$journal_size)) {
      base_style$top_row <- stringr::str_replace(
        string = base_style$top_row,
        pattern = "font-size:.*rem;",
        replacement = paste0("font-size: ", custom_style$journal_size, ";")
      )
    }
    if (!is.null(custom_style$journal_font)) {
      base_style$top_row <- stringr::str_replace(
        string = base_style$top_row,
        pattern = "font-family:(?:[^;]*;){1}",
        replacement = paste0("font-family: ", custom_style$journal_font, ", sans-serif;")
      )
    }
    if (!is.null(custom_style$journal_weight)) {
      base_style$top_row <- stringr::str_replace(
        string = base_style$top_row,
        pattern = "font-weight:(?:[^;]*;){1}",
        replacement = paste0("font-weight: ", custom_style$journal_weight, ";")
      )
    }
    if (!is.null(custom_style$journal_color)) {
      base_style$top_row <- stringr::str_replace(
        string = base_style$top_row,
        pattern = "color:(?:[^;]*;){1}",
        replacement = paste0("color: ", custom_style$journal_color, ";")
      )
    }

    ## TITLE ROW
    if (!is.null(custom_style$title_size)) {
      base_style$title_row <- stringr::str_replace(
        string = base_style$title_row,
        pattern = "font-size:.*rem;",
        replacement = paste0("font-size: ", custom_style$title_size, ";")
      )
    }
    if (!is.null(custom_style$title_font)) {
      base_style$title_row <- stringr::str_replace(
        string = base_style$title_row,
        pattern = "font-family:(?:[^;]*;){1}",
        replacement = paste0("font-family: ", custom_style$title_font, ", sans-serif;")
      )
    }
    if (!is.null(custom_style$title_weight)) {
      base_style$title_row <- stringr::str_replace(
        string = base_style$title_row,
        pattern = "font-weight:(?:[^;]*;){1}",
        replacement = paste0("font-weight: ", custom_style$title_weight, ";")
      )
    }
    if (!is.null(custom_style$title_color)) {
      base_style$title_row <- stringr::str_replace(
        string = base_style$title_row,
        pattern = "color:(?:[^;]*;){1}",
        replacement = paste0("color: ", custom_style$title_color, ";")
      )
    }

    ## AUTHOR ROW
    if (!is.null(custom_style$author_size)) {
      base_style$author_row <- stringr::str_replace(
        string = base_style$author_row,
        pattern = "font-size:.*rem;",
        replacement = paste0("font-size: ", custom_style$author_size, ";")
      )
    }
    if (!is.null(custom_style$author_font)) {
      base_style$author_row <- stringr::str_replace(
        string = base_style$author_row,
        pattern = "font-family:(?:[^;]*;){1}",
        replacement = paste0("font-family: ", custom_style$author_font, ", sans-serif;")
      )
    }
    if (!is.null(custom_style$author_weight)) {
      base_style$author_row <- stringr::str_replace(
        string = base_style$author_row,
        pattern = "font-weight:(?:[^;]*;){1}",
        replacement = paste0("font-weight: ", custom_style$author_weight, ";")
      )
    }
    if (!is.null(custom_style$author_color)) {
      base_style$author_row <- stringr::str_replace(
        string = base_style$author_row,
        pattern = "color:(?:[^;]*;){1}",
        replacement = paste0("color: ", custom_style$author_color, ";")
      )
    }

    # assign inline CSS strings to the respective list items according to the style argument
    css_styles$top_row_style <- base_style$top_row
    css_styles$title_row_style <- base_style$title_row
    css_styles$author_row_style <- base_style$author_row
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
