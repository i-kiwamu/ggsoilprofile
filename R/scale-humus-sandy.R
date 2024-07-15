#' scale_humus_sandy_identity
#' @export
scale_humus_sandy_identity <-
  function(...,
           choices = c("little", "common", "rich", "very rich", "sandy"),
           guide = "legend",
           na.value = "little") {
    scale_pattern <- ggpattern::scale_pattern_manual(
      values = c("little" = "none", "common" = "stripe",
                 "rich" = "stripe", "very rich" = "crosshatch",
                 "sandy" = "circle"),
      na.value = "none")
    scale_pattern_linetype <- ggpattern::scale_pattern_linetype_manual(
      values = c("little" = "blank", "common" = "dotted",
                 "rich" = "solid", "very rich" = "solid",
                 "sandy" = "blank"),
      na.value = "blank")
    scale_pattern_density <- ggpattern::scale_pattern_density_manual(
      values = c("little" = 0.01, "common" = 0.01,
                 "rich" = 0.01, "very rich" = 0.01,
                 "sandy" = 0.3),
      na.value = 0.01)
    return(list(scale_pattern, scale_pattern_linetype, scale_pattern_density))
  }
