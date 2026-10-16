#' Carve a smiling face
#'
#' @param image A ggplot object.
#'
#' This function carves a smiling face in the jack-o-lantern.
#'
#' @examples
#' image <- create_background() |> grow_pumpkin()
#' carve_smile(image)
#'

carve_smile <- function(image) {
  # bounds: x = 300:700, y = 180:250
  vertical_offset <- -40

  # eyes - mirror ----
  eye_left <- data.frame(
    x = c(0, 40, 50, 65, 90, 100, 120, 140, 120, 100, 90, 50, 20) + 290,
    y = c(0, 5, 30, 40, 40, 35, 5, 0, 80, 100, 105, 95, 70) + 310 + vertical_offset
  )
  eye_right <- eye_left
  eye_right$x = 1000 - eye_right$x

  # mouth - mirrors itself ----
  mouth_left <- data.frame(
    x = c(500, 450, 430, 410, 390, 350, 320, 300, 320, 340, 350, 360, 390,
          420, 430, 460, 490, 500),
    y = c(180, 180, 190, 220, 200, 220, 230, 250, 250, 240, 250, 250, 230,
          250, 250, 220, 250, 250) + vertical_offset,
    id = 1:18
  )
  mouth_right <- mouth_left
  mouth_right$x = 1000 - mouth_right$x
  mouth_right <- mouth_right[order(mouth_right$id, decreasing = TRUE), ]
  mouth_right <- mouth_right[!mouth_right$x == 500, ]
  mouth <- rbind(mouth_left, mouth_right)
  rm(mouth_left, mouth_right)

  # add face to image ----
  p <- image +
    ggforce::geom_shape(eye_left,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.012, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_shape(eye_right,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.012, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_shape(mouth,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.003, "native"),
                        color = "yellow", fill = "yellow")

  # return image ----
  return(p)
}
