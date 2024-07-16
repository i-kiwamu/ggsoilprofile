#' GeomGravel
#' @description A \code{ggproto} object for gravel.
GeomGravel <- ggplot2::ggproto(
  "GeomGravel", ggplot2::GeomPoint,
  required_aes = c("x", "top", "bottom"),
  no_missing_aes = c("colour", "size", "graveltype", "gravelabund"),
  extra_params = c("just", "na.rm", "orientation", "width"),
  default_aes = ggplot2::aes(colour = "black", size = 3, fill = NA,
                             alpha = NA, stroke = 0.5,
                             graveltype = "round", gravelabund = "none"),
  setup_data = function(data, params) {
    data$shape <- graveltype2shape(data$graveltype %||% params$graveltype %||% "round")
    data$width <- data$width %||%
      params$width %||%
      (min(vapply(split(data$x, data$PANEL, drop = TRUE),
                  ggplot2::resolution, numeric(1), zero = FALSE)) * 0.9)
    data$just <- params$just %||% 0.5
    data <- transform(data,
                      y = top + (bottom - top) / 2) %>%
      dplyr::mutate(freq = dplyr::case_when(gravelabund == "common" ~ 1,
                                            gravelabund == "rich" ~ 2,
                                            gravelabund == "very rich" ~ 3,
                                            .default = 0)) %>%
      tidyr::uncount(freq, .remove = FALSE, .id = "id") %>%
      transform(x = x + (id - 0.5 * (freq + 1)) * width * just * 0.5)
    data
  }
)


#' graveltype2shape
#' @param gravel_type A character vector of graveltype.
#' @return An integer vector of shapes.
#' @export
graveltype2shape <- function(gravel_type) {
  shape_table <- c(
    "none" = 1,
    "round" = 1,
    "subangular" = 0,
    "angular" = 2,
    "pumice" = 5
  )

  gravel_match <- charmatch(gravel_type, names(shape_table))
  invalid_strings <- is.na(gravel_match)
  nonunique_strings <- gravel_match == 0

  if (any(invalid_strings)) {
    bad_string <- unique0(gravel_type[invalid_strings])
    cli::cli_abort("Graveltype aesthetic contains invalid value{?s}: {.val {bad_string}}.")
  }
  if(any(nonunique_strings)) {
    bad_string <- unique0(gravel_type[nonunique_string])
    cli::cli_abort("Graveltype must be given unambiguously,",
                   "i" = "Fix {.val {bad_string}}.")
  }

  unname(shape_table[gravel_type])
}

#' geom_gravel
#' @description A \code{geom} object for gravel.
#' @inheritParams ggplot2::geom_point
geom_gravel <-
  function(mapping = NULL, data = NULL, stat = "identity",
           position = "stack", ...,
           just = 0.5, width = NULL, na.rm = TRUE, orientation = NA,
           show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
      geom = GeomGravel, mapping = mapping, data = data, stat = stat,
      position = position, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list2(just = just, width = width, na.rm = na.rm,
                     orientation = orientation, ...)
    )
  }
