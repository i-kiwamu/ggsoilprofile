library(ggplot2)

test_that("multiplication works", {
  fig1 <- pedons %>%
    ggplot(aes(pedon, top, yend = bottom, group = paste(pedon, top, bottom))) +
    geom_horizon(aes(fill = color)) +
    scale_x_discrete(position = "top") +
    scale_y_reverse("Depth (cm)") +
    scale_fill_manual(values = mnsl2hex(sort(unique(pedons$color))),
                      na.value = "transparent", guide = "none")
  expect_no_error(fig1)
})
