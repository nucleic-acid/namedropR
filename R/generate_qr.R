#' @title generate_qr
#'
#' @description Generates a QR code from a supplied string and return as a plot object.
#' This is a wrapper function around qrcode::qr_code().
#'
#' @param url A string to encode as QR code.
#'
#' @return The encoded QR code as matrix.
#'
#' @importFrom qrcode qr_code


generate_qr <- function(url) {

  # CHECK INPUTS
  stopifnot(class(url) == "character")

  # create QR-Code
  qr <- qrcode::qr_code(url)

  return(qr)
}
