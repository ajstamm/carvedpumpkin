#' Grow the pumpkin
#'
#' @description
#' Sorry, all pumpkins are cloned from the same stock, so there is no variation.
#'

create_pumpkin <- function() {
  # backdrop ----
  sky_gradient <- grDevices::colorRampPalette(c("darkgreen", "blue4",
                                                "purple4"))(100)
  bg <- tibble::tibble(x = (0:100) * 10, y = (0:100) * 7)
  stars <- tibble::tibble(x = sample(seq(0, 1000), 500, replace = TRUE),
                          y = sample(seq(250, 700), 500, replace = TRUE),
                          vals = factor(sample(1:10, 500, replace = TRUE)))
  stars$x100 <- floor(stars$x / 100)
  stars$y100 <- floor(stars$y / 100)

  # stem ----
  stem <- data.frame(
    id = rep(1:3, each = 4),
    x = c(470, 480, 490, 485,  485, 490, 505, 515,  515, 505, 515, 530),
    y = c(480, 520, 524, 480,  480, 524, 526, 480,  480, 526, 530, 480),
    vals = factor(rep(c(1,2,1), each = 4)), type = 2
  )

  # plot object ----
  graphics::par(bg = "black", mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))
  # background ----
  p <- ggplot2::ggplot(stars, ggplot2::aes(x = !!dplyr::sym("x"),
                                           y = !!dplyr::sym("y"))) +
    ggplot2::theme_classic() +
    ggplot2::geom_line() +
    mapply(function(ylow, yhigh, col, a = 1) {
      ggplot2::geom_ribbon(ggplot2::aes(ymin = ylow, ymax = yhigh),
                           alpha = a, fill = col)
    }, utils::head(bg$y, -1), utils::tail(bg$y, -1), sky_gradient,
    seq(1, 700, length = 100)) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   legend.position = "none")
  p <- p + # stars
    ggplot2::geom_point(size = 1, colour = "white", shape = ".", alpha = 1)
  # pumpkin ----
  p <- p + # pumpkin: stem
    ggplot2::geom_polygon(stem, mapping =
                            ggplot2::aes(x = !!dplyr::sym("x"),
                                         y = !!dplyr::sym("y"),
                                         fill = !!dplyr::sym("vals"),
                                         group = !!dplyr::sym("vals"))) +
    ggplot2::scale_fill_manual(values = c("chartreuse4", "mediumseagreen")) +
    # pumpkin body: outside edges
    ggforce::geom_ellipse(color = "chocolate4", fill = "darkorange3",
                          ggplot2::aes(x0 = 350, y0 = 300, a = 250, b = 200,
                                       angle = 0)) +
    ggforce::geom_ellipse(color = "chocolate4", fill = "darkorange3",
                          ggplot2::aes(x0 = 650, y0 = 300, a = 250, b = 200,
                                       angle = 0)) +
    # pumpkin body: next layer
    ggforce::geom_ellipse(color = "chocolate3", fill = "darkorange2",
                          ggplot2::aes(x0 = 400, y0 = 300, a = 180, b = 200,
                                       angle = 0)) +
    ggforce::geom_ellipse(color = "chocolate3", fill = "darkorange2",
                          ggplot2::aes(x0 = 600, y0 = 300, a = 180, b = 200,
                                       angle = 0)) +
    # pumpkin body: middle layer
    ggforce::geom_ellipse(color = "chocolate2", fill = "darkorange",
                          ggplot2::aes(x0 = 500, y0 = 300, a = 150, b = 200,
                                       angle = 0)) +
    ggplot2::coord_fixed()
  # return image ----
  return(p)
}
