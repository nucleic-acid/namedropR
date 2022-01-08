#' @title write_vc
#'
#' @description Takes the data from drop_name and writes the actual outputs to the output directory.
#'
#'
#'
#' @examples
#' \dontrun{
#'
#' }
#'
#' @import bib2df
#' @import dplyr
#' @importFrom htmltools tags save_html
#' @importFrom here here
#' @importFrom lubridate year ymd
#' @importFrom webshot webshot

write_vc <- function(work_item, path_absolute, output_dir, export_as) {

  # EXPORT RESULT(s)

  if (path_absolute) {
    output_path <- here::here(output_dir, paste0(work_item$BIBTEXKEY, ".html"))
  } else {
    output_path <- file.path(output_dir, paste0(work_item$BIBTEXKEY, ".html"))
  }

  if (export_as == "html_full") {
    tryCatch(
      expr = {
        htmltools::save_html(work_item$vcs, file = here::here(output_path))
      },
      error = function(e) {
        message("Could not save the HTML output:")
        print(e)
      },
      warning = function(w) {
        message("Having difficulties saving the HTML output:")
        print(w)
      }
    )
  } else if (export_as == "html") {
    tryCatch(
      expr = {
        write(as.character(work_item$vcs), file = here::here(output_path))
      },
      error = function(e) {
        message("Could not save the HTML output:")
        print(e)
      },
      warning = function(w) {
        message("Having difficulties saving the HTML output:")
        print(w)
      }
    )
  } else if (export_as == "png") {
    if (!webshot::is_phantomjs_installed()) {
      message("You need to download and install phantomJS to save output as PNG. Try running 'webshot::install_phantomjs()' once.")
    } else {
      # renders as "complete" html to get the white background for PNG snapshot.
      tryCatch(
        expr = {
          htmltools::save_html(work_item$vcs, file = here::here(output_path))
        },
        error = function(e) {
          message("Could not save the intermediate HTML output:")
          print(e)
        },
        warning = function(w) {
          message("Having difficulties saving the intermediate HTML output:")
          print(w)
        }
      )

      tryCatch(
        expr = {
          webshot::webshot(output_path, paste0(output_path, ".png"), selector = ".visual-citation", zoom = 2)
        },
        error = function(e) {
          message("Could not take a screenshot of the intermediate HTML.")
          print(e)
        },
        warning = function(w) {
          message("Having difficulties taking a screenshot of the intermediate HTML output:")
          print(w)
        }
      )
      unlink(output_path)
      # to point to the png instead return its filepath
      return(paste0(output_path, ".png"))
    }
  } else {
    stop("Output format unknown")
  }
  return(as.character(output_path))
}
