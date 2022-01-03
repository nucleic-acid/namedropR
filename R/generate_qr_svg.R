#' @title generate_qr_svg
#'
#' @description Generates a QR code from a supplied string and temporarily saves it in the tempdir.
#'
#' @param url A string to encode as QR code.
#' @param cite_key A string containing the cite_key to allow for correct file naming.
#'
#' @return Nothing. The encoded QR code is stored to disk.
#'
#' @examples
#' \dontrun{
#' generate_qr_svg("https://doi.org/10.1007/bf00396886", cite_key = "Eschrich1983")
#' }
#'
#' @importFrom qrcode qr_code generate_svg


generate_qr_svg <- function(url, cite_key) {

  # CHECK INPUTS
  stopifnot(class(url) == "character")
  stopifnot(class(cite_key) == "character")

  # create QR-Code
  doi_qr <- qrcode::qr_code(url)

  # store QR-code in tempdir()
  tryCatch(
    expr = {
      qrcode::generate_svg(doi_qr, filename = paste0(tempdir(),"/qr_codes/qr_",cite_key, ".svg"), show = FALSE)
    }
    error = function(e){
      warning("Something went wrong creating the QR code for ", cite_key, "! No QR code will be stored to file.")
      print(e)
    })
}
