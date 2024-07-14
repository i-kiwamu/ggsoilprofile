#' aes_horizon
#' @description aes() for soil horizon.
#' @param x x axis.
#' @param top Depth of top boundary.
#' @param bottom Depth of bottom boundary.
#' @param ... Other arguments.
#' @return \code{ggplot2::aes} object.
#' @export
aes_horizon <- function(x, top, bottom, ...) {
  mapping <- ggplot2::aes(x = {{x}}, top = {{top}}, bottom = {{bottom}}, ...)
  mapping$group <-
    as_quosure(substitute(paste(a, t, b),
                          list(a = quo_get_expr(mapping$x),
                               t = quo_get_expr(mapping$top),
                               b = quo_get_expr(mapping$bottom))),
               parent.frame())
  return(mapping)
}
