#' Grow the turnip
#'
#' @param bg Plot object; backdrop for the image
#'
#' @description
#' All turnips are cloned from the same stock, so there is no variation.
#'
#' @examples
#' create_background() |> grow_turnip()
#' # bg <- create_background()
#'

grow_turnip <- function(bg) {
  # size: 560x400?
  vertical_offset <- 0
  y_ellipse <- 260 + vertical_offset

  # define the stem ----
  stem <- data.frame(
    x = c(480, 495, 495, 505,  505, 520),
    y = c(450, 470, 510, 510,  470, 450) + vertical_offset
  )
  # define colors ----
  my_pal <- c("maroon4", "maroon4", "cornsilk1")
  my_gradient <- grid::linearGradient(my_pal,
                                      x1 = grid::unit(.5, "npc"),
                                      x2 = grid::unit(.5, "npc"),
                                      y1 = grid::unit(1, "npc"),
                                      y2 = grid::unit(0, "npc"))

  # plot the turnip ----
  p <- bg +
    ggforce::geom_shape(stem,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.003, "native"),
                        color = "darkmagenta", fill = "maroon4") +
    ggforce::geom_ellipse(color = "darkmagenta", fill = my_gradient,
                          ggplot2::aes(x0 = 500, y0 = y_ellipse,
                                       a = 260, b = 200, angle = 0)) +
    ggplot2::coord_fixed()
  # return image ----
  return(p)
}
