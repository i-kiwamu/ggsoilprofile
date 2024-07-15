#' mnsl2hex
#' @description Convert from Munsell color to RGB hex.
#' @param mnsl character of Munsell color (ex 10YR 4/2, N 3/0)
#' @return character of RGB hex (ex #6D5D4A, #474747)
#' @export
mnsl2hex <- function(mnsl) {
  mnsl <- ifelse(is.na(mnsl), "N 10", mnsl)
  df_rgb <- munsellinterpol::MunsellToRGB(mnsl)$RGB
  hex <- rgb(df_rgb[,"R"], df_rgb[,"G"], df_rgb[,"B"], maxColorValue = 255)
  return(hex)
}
