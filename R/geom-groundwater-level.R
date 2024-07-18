#' GeomGroundwaterLevel
#' @description A \code{ggproto} object for groundwater level.
GeomGroundwaterLevel <- ggplot2::ggproto(
  "GeomGroundwaterLevel", ggplot2::GeomText,
  required_aes = c("x", "top", "bottom"),
  default_aes = ggplot2::aes(colour = "black", size = 3.88, angle = 0, hjust = 0.5,
                             vjust = 0.5, alpha = NA, family = "", fontface = 1,
                             lineheight = 1.2, depth = NA),
  setup_data = function(data, params) {
    height <- max(data$bottom) - min(data$top)
    data$width <- data$width %||%
      params$width %||%
      (min(vapply(split(data$x, data$PANEL, drop = TRUE),
                  ggplot2::resolution, numeric(1), zero = FALSE)) * 0.9)
    data$just <- params$just %||% 0.5
    data <- data %>%
      dplyr::filter(!is.na(depth)) %>%
      transform(x = x - 0.5 * width * just,
                freq = 5) %>%
      tidyr::uncount(freq, .id = "id") %>%
      transform(y = depth + (id - 3) * height * 0.005,
                vjust = dplyr::case_match(id,
                                          1 ~ -0.5,
                                          c(2:5) ~ 0.5),
                label = dplyr::case_match(id,
                                          1 ~ as.character(depth),
                                          2 ~ "\U25BC",
                                          3 ~ "\U2500\U2500",
                                          4 ~ "\U2500",
                                          5 ~ "\U2013"))
  }
)


#' geom_groundwater_level
#' @description A \code{geom} object for groundwater level symbol.
#' @inheritParams ggplot2::geom_text
geom_groundwater_level <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "identity", na.rm = FALSE, show.legend = NA,
           inherit.aes = TRUE, ...) {
    ggplot2::layer(
      geom = GeomGroundwaterLevel, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(na.rm = na.rm, ...)
    )
  }
