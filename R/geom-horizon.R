#' GeomHorizonLine
#' @description A \code{ggproto} object for horizon line.
#' @importFrom dplyr group_by filter ungroup
GeomHorizonLine <- ggplot2::ggproto(
  "GeomHorizonLine", ggplot2::GeomSegment,
  required_aes = c("x", "y", "yend"),
  default_aes = ggplot2::aes(linetype = "solid", linewidth = 0.5,
                             colour = "black", alpha = NA),
  setup_data = function(data, params) {
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||%
      (min(vapply(split(data$x, data$PANEL, drop = TRUE),
                  ggplot2::resolution,
                  numeric(1), zero = FALSE)) * 0.9)
    data$just <- params$just %||% 0.5
    data_left <- transform(data,
                           x = x - width * just,
                           xend = x - width * just)
    data_right <- transform(data,
                            x = x + width * (1 - just),
                            xend = x + width * (1 - just))
    data_top <- data %>%
      group_by(x) %>%
      filter(y == max(y)) %>%
      ungroup() %>%
      transform(x = x - width * just,
                xend = x + width * (1 - just),
                yend = y)
    data_inside <- data %>%
      group_by(x) %>%
      filter(y != max(y)) %>%
      ungroup() %>%
      transform(x = x - width * just,
                xend = x + width * (1 - just),
                yend = y)
    ggplot2::flip_data(rbind(data_left, data_right, data_top, data_inside),
                             params$flipped_aes)
  }
)


#' geom_horizon_line
#' @description A \code{geom} object for horizon line.
#' @inheritParams ggplot2::geom_segment
geom_horizon_line <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "identity", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHorizonLine, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}


#' GeomHorizonLine
#' @description A \code{ggproto} object for horizon line.
GeomHorizonInside <- ggplot2::ggproto(
  "GeomHorizonInside", ggplot2::GeomBar,
  required_aes = c("x", "y", "yend"),
  default_aes = ggplot2::aes(linetype = "blank", linewidth = 0,
                             colour = NA, fill = "transparent", alpha = NA),
  setup_data = function(data, params) {
    data <- transform(data[order(data$x, data$y, decreasing = TRUE),],
                      y = yend - y)
    data$flipped_aes <- params$flipped_aes
    data <- ggplot2::flip_data(data, params$flipped_aes)
    data$width <- data$width %||%
      params$width %||%
      (min(vapply(split(data$x, data$PANEL, drop = TRUE),
                  ggplot2::resolution, numeric(1), zero = FALSE)) * 0.9)
    data$just <- params$just %||% 0.5
    data <- transform(data,
                      ymin = pmin(y, 0), ymax = pmax(y, 0),
                      xmin = x - width * just, xmax = x + width * (1 - just),
                      width = NULL, just = NULL)
    ggplot2::flip_data(data, params$flipped_aes)
  }
)


#' geom_horizon_inside
#' @description A \code{geom} object for horizon inside.
#' @inheritParams ggplot2::geom_bar
geom_horizon_inside <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", na.rm = TRUE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = GeomHorizonInside, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }


#' geom_horizon
#' @description A \code{geom} object for horizon.
#' @inheritParams ggplot2::geom_bar
#' @export
geom_horizon <- function(mapping = NULL, data = NULL, stat = "identity",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  list(geom_horizon_inside(mapping = mapping, data = data, stat = stat,
                           position = ggplot2::position_stack(reverse = TRUE),
                           na.rm = na.rm,
                           show.legend = show.legend, inherit.aes = inherit.aes,
                           ...),
       geom_horizon_line(mapping = mapping[names(mapping) != "fill"],
                         data = data, stat = stat,
                         position = "identity", na.rm = na.rm,
                         show.legend = show.legend, inherit.aes = inherit.aes,
                         ...))
}
