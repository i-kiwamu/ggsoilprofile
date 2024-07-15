#' scale_boundary_identity
#' @export
scale_boundary_identity <-
  function(...,
           choices = c("very abrupt", "abrupt", "clear", "gradual"),
           guide = "legend",
           na.value = "abrupt") {
    scale_linetype <- ggplot2::scale_linetype_manual(
      values = c("very abrupt" = "solid", "abrupt" = "solid", "clear" = "dotdash",
                 "gradual" = "dotted"),
      na.value = "solid")
    scale_linewidth <- ggplot2::scale_linewidth_manual(
      values = c("very abrupt" = 1, "abrupt" = 0.5, "clear" = 0.5,
                 "gradual" = 0.5),
      na.value = 0.5)
    return(list(scale_linetype, scale_linewidth))
}
