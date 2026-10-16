#' Carve a bat
#'
#' @param image Plot object; background and vegetable.
#'
#' This function carves a bat in the jack-o-lantern.
#'
#' @examples
#' create_background() |> grow_turnip() |> carve_bat()
#'

carve_bat <- function(image) {
  # bounds: x = 300:700, y = 180:250
  vertical_offset <- -40

  # wings - mirror ----
  wing_left <- data.frame(
    x = c(12, 9, 8, 7, 6, 3, 0, 2, 4, 5, 6, 9, 10, 11, 14, 17, 18) * 11 + 300,
    y = c(16, 17, 18, 20, 19, 18, 18, 17, 15, 11, 11, 9, 7, 7, 6, 4, 0) *
        10 + 160 + vertical_offset,
    id = 1:17
  )
  wing_right <- wing_left
  wing_right$x = 1000 - wing_right$x
  wing_right <- wing_right[order(wing_right$id, decreasing = TRUE), ]
  wings <- rbind(wing_left, wing_right)
  rm(wing_left, wing_right)

  # ears - mirror ----
  ear_left <- data.frame(
    x = c(11, 11, 12) * 10 + 360,
    y = c(18, 22, 20) * 10 + 180 + vertical_offset
  )
  ear_right <- ear_left
  ear_right$x = 1000 - ear_right$x

  # add bat to image ----
  p <- image +
    ggforce::geom_shape(wings,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.003, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_ellipse(color = "yellow", fill = "yellow",
                          ggplot2::aes(x0 = 500, y0 = 360 + vertical_offset,
                                       a = 30, b = 30, angle = 0)) +
    ggforce::geom_shape(ear_left,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_shape(ear_right,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0, "native"),
                        color = "yellow", fill = "yellow")

  # return image ----
  return(p)
}
