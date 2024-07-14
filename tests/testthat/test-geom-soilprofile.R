library(ggplot2)

test_that("multiplication works", {
  fig1 <- ggplot(pedons, aes(pedon, top = top, bottom = bottom, group = paste(pedon, top, bottom))) +
    geom_horizon(aes(fill = color)) +
    scale_x_discrete(position = "top") +
    ylab("Depth (cm)") +
    coord_trans(y = "reverse")
  expect_no_error(fig1)
})
