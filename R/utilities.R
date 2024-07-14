mnsl2hex <- function(mnsl) {
  mnsl <- ifelse(is.na(mnsl), "N 0/0", mnsl)
  df_rgb <- munsellinterpol::MunsellToRGB(mnsl)$RGB
  hex <- rgb(df_rgb[,"R"], df_rgb[,"G"], df_rgb[,"B"], maxColorValue = 255)
  return(hex)
}
