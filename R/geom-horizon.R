#' GeomHorizonLine
#' @description A \code{ggproto} object for horizon line.
#' @importFrom dplyr group_by filter ungroup
GeomHorizonLine <- ggplot2::ggproto(
  "GeomHorizonLine", ggplot2::Geom,
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
      filter(top == max(top)) %>%
      ungroup() %>%
      transform(x = x - width * just,
                xend = x + width * (1 - just),
                y = top,
                yend = top)
    data_inside <- data %>%
      group_by(x) %>%
      filter(top != max(top)) %>%
      ungroup() %>%
      transform(x = x - width * just,
                xend = x + width * (1 - just),
                y = top,
                yend = top)
    ggplot2::flip_data(rbind(data_left, data_right, data_top, data_inside),
                             params$flipped_aes)
  },
  draw_panel = ggplot2::GeomSegment$draw_panel,
  draw_key = ggplot2::draw_key_path,
  rename_size = TRUE
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


#' GeomHorizonMunsell
#' @description A \code{ggproto} object for horizon with munsell.
GeomHorizonMunsell <- ggplot2::ggproto(
  "GeomHorizonMunsell", ggplot2::GeomBar,
  required_aes = c("x", "top", "bottom"),
  default_aes = ggplot2::aes(linetype = "blank", linewidth = 0,
                             colour = NA, fill = "transparent", alpha = NA),
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


#' geom_horizon_munsell
#' @description A \code{geom} object for horizon inside with munsell.
#' @inheritParams ggplot2::geom_bar
geom_horizon_munsell <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", na.rm = TRUE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = GeomHorizonMunsell, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }


pattern_aesthetics <- ggplot2::aes(
  pattern                  = 'stripe',
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


#' GeomHorizonHumus
#' @description A \code{ggproto} object for horizon with humus.
GeomHorizonHumus <- ggplot2::ggproto(
  "GeomHorizonHumus", ggpattern::GeomBarPattern,
  required_aes = c("x", "top", "bottom"),
  default_aes = c(ggplot2::aes(linetype = "blank", linewidth = 0,
                               colour = NA, fill = "white", alpha = NA),
                  pattern_aesthetics),
  setup_data = function(data, params) {
    data <- transform(data[order(data$x, data$top, decreasing = TRUE),],
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


#' geom_horizon_humus
#' @description A \code{geom} object for horizon inside with humus.
#' @inheritParams ggpattern::geom_bar_pattern
geom_horizon_humus <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", ...,
           just = 0.5, width = NULL, na.rm = TRUE, orientation = NA,
           show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
      geom = GeomHorizonHumus, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list2(just = just, width = width, na.rm = na.rm,
                     orientation = orientation, ...)
    )
  }


#' geom_horizon
#' @description A \code{geom} object for horizon.
#' @inheritParams ggplot2::geom_bar
#' @export
geom_horizon <- function(mapping = NULL, data = NULL, stat = "identity",
                         na.rm = FALSE, show.legend = NA,
                         inherit.aes = TRUE, ...) {
  if ("fill" %in% names(mapping)) {
    list(geom_horizon_munsell(mapping = mapping, data = data, stat = stat,
                              position = ggplot2::position_stack(reverse = TRUE),
                              na.rm = na.rm, show.legend = show.legend,
                              inherit.aes = inherit.aes, ...),
         ggplot2::scale_fill_manual(
           values = mnsl2hex(sort(unique(pedons$color))),
           na.value = "transparent", guide = "none"),
         geom_horizon_line(mapping = mapping[names(mapping) != "fill"],
                           data = data, stat = stat,
                           position = "identity", na.rm = na.rm,
                           show.legend = show.legend, inherit.aes = inherit.aes,
                           ...))
  } else if ("pattern" %in% names(mapping)) {
    list(geom_horizon_humus(mapping = mapping, data = data, stat = stat,
                            position = ggplot2::position_stack(reverse = TRUE),
                            ...,
                            na.rm = na.rm, show.legend = show.legend,
                            inherit.aes = inherit.aes),
         ggpattern::scale_pattern_manual(
           values = c("little" = "none", "common" = "stripe",
                      "rich" = "stripe", "very rich" = "crosshatch")),
         ggpattern::scale_pattern_linetype_manual(
           values = c("little" = "blank", "common" = "dotted",
                      "rich" = "solid", "very rich" = "solid")),
         geom_horizon_line(mapping = mapping[setdiff(names(mapping), c("pattern", "pattern_linetype"))],
                           data = data, stat = stat,
                           position = "identity", na.rm = na.rm,
                           show.legend = show.legend, inherit.aes = inherit.aes,
                           ...))
  }
}
