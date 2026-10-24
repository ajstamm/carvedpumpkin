#' Grow the pumpkin
#'
#' @param bg Plot object; backdrop for the image
#'
#' @description
#' All pumpkins are cloned from the same stock, so there is no variation.
#'
#' @examples
#' create_background() |> grow_pumpkin()
#'
#'

grow_pumpkin <- function(bg) {
  vertical_offset <- -40
  y_ellipse <- 300 + vertical_offset

  # define the stem ----
  stem <- data.frame(
    id = rep(1:3, each = 4),
    x = c(470, 480, 490, 485,  485, 490, 505, 515,  515, 505, 515, 530),
    y = c(480, 520, 524, 480,  480, 524, 526, 480,  480, 526, 530, 480) +
        vertical_offset,
    vals = factor(rep(c(1,2,1), each = 4)), type = 2
  )

  my_pal <- c("chocolate3", "darkorange2", "darkorange")
  my_gradient <- grid::radialGradient(my_pal,
                                      cx1 = grid::unit(.5, "npc"),
                                      cx2 = grid::unit(.5, "npc"),
                                      cy1 = grid::unit(.6, "npc"),
                                      cy2 = grid::unit(.4, "npc"),
                                      r2 = grid::unit(1, "npc"),
                                      extend = "none")


  # plot the pumpkin ----
  p <- bg + # pumpkin: stem
    ggplot2::geom_polygon(stem, mapping =
                            ggplot2::aes(x = !!dplyr::sym("x"),
                                         y = !!dplyr::sym("y"),
                                         fill = !!dplyr::sym("vals"),
                                         group = !!dplyr::sym("vals"))) +
    ggplot2::scale_fill_manual(values = c("chartreuse4", "mediumseagreen")) +
    # pumpkin body: outside edges
    ggforce::geom_ellipse(color = "chocolate4", fill = my_gradient,
                          ggplot2::aes(x0 = 350, y0 = y_ellipse,
                                       a = 250, b = 200, angle = 0)) +
    ggforce::geom_ellipse(color = "chocolate4", fill = my_gradient,
                          ggplot2::aes(x0 = 650, y0 = y_ellipse,
                                       a = 250, b = 200, angle = 0)) +
    # pumpkin body: next layer
    ggforce::geom_ellipse(color = "chocolate3", fill = my_gradient,
                          ggplot2::aes(x0 = 400, y0 = y_ellipse,
                                       a = 180, b = 200, angle = 0)) +
    ggforce::geom_ellipse(color = "chocolate3", fill = my_gradient,
                          ggplot2::aes(x0 = 600, y0 = y_ellipse,
                                       a = 180, b = 200, angle = 0)) +
    # pumpkin body: middle layer
    ggforce::geom_ellipse(color = "chocolate3", fill = my_gradient,
                          ggplot2::aes(x0 = 500, y0 = y_ellipse,
                                       a = 150, b = 200, angle = 0)) +
    ggplot2::coord_fixed()
  # return image ----
  return(p)
}
