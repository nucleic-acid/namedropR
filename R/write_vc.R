#' @title write_vc
#'
#' @description Takes the data from drop_name and writes the actual outputs to the output directory.
#'
#' @param work_item A data.frame or tibble with a single row, passed by drop_name()
#' @param output_dir A relative path (in regard to the working directory) where the visual citations should be 'dropped'. (type: character)
#' @param path_absolute A logical parameter specifying, whether eventually a relative or absolute path should be returned.
#' @param export_as Defines the file format of the returned visual citation (see drop_name() for more).
#' @return The path to the written file as character.
#'
#' @examples
#' \dontrun{
#' # not intended for direct call. Please refer to the documentation
#' # of drop_name() for further assistance.
#' }
#'
#' @importFrom htmltools tags save_html
#' @importFrom webshot webshot

write_vc <- function(work_item, path_absolute, output_dir, export_as) {

  # EXPORT RESULT(s)

  # create output directory, if needed
  if (!dir.exists(output_dir)) {
    tryCatch(
      expr = dir.create(output_dir, recursive = TRUE),
      error = function(e) {
        message("Could not create the output directory. Check file permissions.")
        print(e)
      },
      warning = function(w) {
        message("Having difficulties creating the output directory:")
        print(w)
      }
    )
  }

  output_file <- file.path(output_dir, paste0(work_item$BIBTEXKEY, ".html"))
  # browser()

  if (export_as == "html_full") {
    tryCatch(
      expr = {
        htmltools::save_html(work_item$vcs, file = output_file)
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
    # As work_item$vcs is read as a list, it needs to be interpreted as
    # tag list before converting to character type for writing.
    tryCatch(
      expr = {
        write(
          as.character(htmltools::as.tags(work_item$vcs)),
          file = output_file
        )
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
          htmltools::save_html(work_item$vcs, file = output_file)
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
          webshot::webshot(output_file, paste0(output_file, ".png"), selector = ".visual-citation", zoom = 2)
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
      unlink(output_file)

      # to point to the png instead return its filepath
      png_out <- paste0(output_file, ".png")

      return_path_png <- ifelse(
          path_absolute,
          R.utils::getAbsolutePath(png_out),
          R.utils::getRelativePath(png_out)
          )
      return(return_path_png)
    }
  } else {
    stop("Output format unknown")
  }

  return_path_html <- ifelse(
    path_absolute,
    R.utils::getAbsolutePath(output_file),
    R.utils::getRelativePath(output_file)
  )

  return(return_path_html)
}
