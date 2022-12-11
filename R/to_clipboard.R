
#' Copies the visual citation into the clipboard
#'
#' Copies the visual citation as an image into the clipboard. Mac, Linux and Windows are supported. For Windows Powershell is used. For Linux xclip is required.
#'
#' @param png_path Absolute path to the png file

# Created with inspiration from imageclipr that goes in the opposite direction
# https://github.com/Toniiiio/imageclipr/blob/master/R/insertImageCode.R
# MIT licenced, copyright Andreas Tonio Liebrand, 2020


to_clipboard <- function(png_path) {

  platform <- Sys.info()[1]

  if (platform == "Darwin") { # MAC OS

    script <- paste0(
      "osascript -e \'
        set the clipboard to (read (\"", png_path, "\") as TIFF picture)
        '")

    system(script)
  } else if (platform == "Windows") {
    script <- paste0(
      "powershell -sta \"
      Add-Type -Assembly System.Windows.Forms;
      Add-Type -Assembly System.Drawing;
      $imgpath = '", png_path, "'
      $img = [Drawing.Image]::FromFile($imgpath)
      [Windows.Forms.Clipboard]::SetImage($img)
      \""
    )
    system(script)
  } else if (platform == "Linux") {
    script <- paste0("echo \"<img src='data:image/png;base64,\"$(base64 -w0 \"", png_path, "\")\"' />\" | \
    xclip -selection clipboard -t text/html")

    tryCatch(system(script), error = function(e) {
      stop(
        "Copying the citation to clipboard with xclip failed.
         Please ensure that the required system dependency xclip is installed and can be used."
      )})
  } else {
    stop("OS not recognised, so visual citation could not be copied to the clipboard. However, you can find it in ", R.utils::getRelativePath(png_path))
  }
  return(invisible(TRUE))
}

