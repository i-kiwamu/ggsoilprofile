library(ggplot2)

test_that("multiplication works", {
  fig1 <- ggplot(pedons, aes_horizon(pedon, top, bottom)) +
    geom_pedon(aes(fill = color)) +
    scale_x_discrete(position = "top") +
    ylab("Depth (cm)") +
    coord_trans(y = "reverse")
  fig2 <- ggplot(pedons, aes_horizon(pedon, top, bottom)) +
    geom_pedon(aes(pattern = humus, pattern_linetype = humus)) +
    scale_x_discrete(position = "top") +
    ylab("Depth (cm)") +
    coord_trans(y = "reverse")
  expect_no_error(fig1)
  expect_no_error(fig2)
})
