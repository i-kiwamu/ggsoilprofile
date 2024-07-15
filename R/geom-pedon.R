#' GeomPedonLine
#' @description A \code{ggproto} object for pedon lines.
#' @importFrom dplyr group_by filter ungroup
GeomPedonLine <- ggplot2::ggproto(
  "GeomPedonLine", ggplot2::Geom,
  required_aes = c("x", "top", "bottom"),
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
                           xend = x - width * just,
                           y = top,
                           yend = bottom)
    data_right <- transform(data,
                            x = x + width * (1 - just),
                            xend = x + width * (1 - just),
                            y = top,
                            yend = bottom)
    data_top <- data %>%
      group_by(x) %>%
      filter(top == min(top)) %>%
      ungroup() %>%
      transform(x = x - width * just,
                xend = x + width * (1 - just),
                y = top,
                yend = top)
    ggplot2::flip_data(rbind(data_left, data_right, data_top),
                             params$flipped_aes)
  },
  draw_panel = ggplot2::GeomSegment$draw_panel,
  draw_key = ggplot2::draw_key_path,
  rename_size = TRUE
)


#' geom_pedon_line
#' @description A \code{geom} object for pedon lines.
#' @inheritParams ggplot2::geom_segment
geom_pedon_line <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "identity", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomPedonLine, mapping = mapping, data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
  }


pattern_aesthetics <- ggplot2::aes(
  pattern                  = "none",
  pattern_type             = NA,
  pattern_subtype          = NA,

  pattern_angle            = 45,
  pattern_density          = 0.01,
  pattern_spacing          = 0.015,
  pattern_xoffset          = 0,
  pattern_yoffset          = 0,

  pattern_alpha            = NA,
  pattern_linetype         = 1,
  pattern_size             = 0.3,
  pattern_shape            = 1,
  pattern_colour           = "black",
  pattern_fill             = "black",
  pattern_fill2            = NA,

  pattern_aspect_ratio     = NA,
  pattern_key_scale_factor = 1,

  pattern_filename         = '',
  pattern_gravity          = NA,   # magick::gravity_types()
  pattern_filter           = '',  # magick::filter_types()
  pattern_scale            = 1,
  pattern_orientation      = 'vertical',

  pattern_phase            = 0,
  pattern_frequency        = 0.1,

  pattern_option_1         = 0,
  pattern_option_2         = 0,
  pattern_option_3         = 0,
  pattern_option_4         = 0,
  pattern_option_5         = 0,

  pattern_grid             = 'square',
  pattern_rot              = 0,
  pattern_res              = getOption("ggpattern_res", NA),
  pattern_units            = 'snpc'
)


#' GeomHorizon
#' @description A \code{ggproto} object for horizon.
GeomHorizon <- ggplot2::ggproto(
  "GeomHorizon", ggpattern::GeomBarPattern,
  required_aes = c("x", "top", "bottom"),
  default_aes = c(ggplot2::aes(linetype = "blank", linewidth = 0,
                               colour = NA, fill = "white", alpha = NA,
                               humus = NA),
                  pattern_aesthetics),
  setup_data = function(data, params) {
    data <- transform(data,
                      y = bottom - top)
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
  },
  rename_size = TRUE
)


#' geom_horizon
#' @description A \code{geom} object for horizon.
#' @inheritParams ggpattern::geom_bar_pattern
geom_horizon <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", ...,
           just = 0.5, width = NULL, na.rm = TRUE, orientation = NA,
           show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
      geom = GeomHorizon, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list2(just = just, width = width, na.rm = na.rm,
                     orientation = orientation, ...)
    )
  }


#' geom_pedon
#' @description A \code{geom} object for pedon.
#' @inheritParams ggplot2::geom_bar
#' @export
geom_pedon <- function(mapping = NULL, data = NULL, stat = "identity",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  list(geom_horizon(mapping = mapping[names(mapping) != "boundary_type"],
                    data = data, stat = stat,
                    position = ggplot2::position_stack(reverse = TRUE),
                    ...,
                    na.rm = na.rm, show.legend = show.legend,
                    inherit.aes = inherit.aes),
       ggplot2::scale_fill_identity(
         na.value = "transparent", guide = "none"),
       ggpattern::scale_pattern_manual(
         values = c("little" = "none", "common" = "stripe",
                    "rich" = "stripe", "very rich" = "crosshatch")),
       ggpattern::scale_pattern_linetype_manual(
         values = c("little" = "blank", "common" = "dotted",
                    "rich" = "solid", "very rich" = "solid")),
       geom_pedon_line(
         mapping = mapping[setdiff(names(mapping), c("boundary_type", "fill", "pattern", "pattern_linetype"))],
         data = data, stat = stat, position = "identity", na.rm = na.rm,
         show.legend = show.legend, inherit.aes = inherit.aes, ...),
       geom_boundary(
         mapping = mapping[setdiff(names(mapping), c("fill", "pattern", "pattern_linetype"))],
         data = data, stat = stat, position = "identity", na.rm = na.rm,
         show.legend = show.legend, inherit.aes = inherit.aes, ...),
       scale_boundary_identity()
       )
}
