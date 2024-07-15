library(ggplot2)

test_that("multiplication works", {
  fig1 <- ggplot(pedons, aes_horizon(pedon, top, bottom)) +
    geom_pedon(aes(fill = mnsl2hex(color))) +
    geom_boundary(aes(boundary_type = boundary)) +
    scale_fill_identity(guide = "none") +
    scale_boundary_identity() +
    scale_x_discrete(position = "top") +
    ylab("Depth (cm)") +
    coord_trans(y = "reverse")

  fig2 <- ggplot(pedons, aes_horizon(pedon, top, bottom)) +
    geom_pedon(aes(humus = humus)) +
    geom_boundary(aes(boundary_type = boundary)) +
    scale_humus_sandy_identity() +
    scale_boundary_identity() +
    scale_x_discrete(position = "top") +
    ylab("Depth (cm)") +
    coord_trans(y = "reverse")

  expect_no_error(fig1)
  expect_no_error(fig2)
})
