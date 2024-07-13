

GeomHorizon <- ggplot2::ggproto(
  "GeomHorizon", ggplot2::GeomSegment,
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
      dplyr::group_by(x) %>%
      dplyr::filter(y == max(y)) %>%
      dplyr::ungroup() %>%
      transform(x = x - width * just,
                xend = x + width * (1 - just),
                yend = y)
    ggplot2::flip_data(rbind(data_left, data_right, data_top),
                       params$flipped_aes)
  }
)
geom_horizon <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomHorizon, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
