#' GeomBoundary
#' @description A \code{ggproto} object for boundary lines between horizons.
#' @importFrom dplyr group_by filter ungroup
GeomBoundary <- ggplot2::ggproto(
  "GeomBoundary", ggplot2::Geom,
  required_aes = c("x", "top", "bottom"),
  default_aes = ggplot2::aes(boundary_type = NULL,
                             linetype = "solid", linewidth = 0.5,
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
    data <- data %>%
      group_by(x) %>%
      filter(top != max(top)) %>%
      ungroup() %>%
      transform(x = x - width * just,
                xend = x + width * (1 - just),
                y = bottom,
                yend = bottom,
                linetype = ifelse(is.na(boundary_type), "solid", boundary_type),
                linewidth = ifelse(is.na(boundary_type), 0.3, boundary_type))
    ggplot2::flip_data(data,
                       params$flipped_aes)
  },
  draw_panel = ggplot2::GeomSegment$draw_panel,
  draw_key = ggplot2::draw_key_path,
  rename_size = TRUE
)


#' geom_boundary
#' @description A \code{geom} object for boundary line between horizons.
#' @inheritParams ggplot2::geom_segment
geom_boundary <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "identity", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = GeomBoundary, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
