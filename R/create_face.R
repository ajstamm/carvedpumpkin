#' Carve a smiling face
#'
#' @param image A ggplot object.
#'
#'

create_face <- function(image) {
  # eyes - mirror ----
  eye_left <- data.frame(
    x = c(0, 40, 50, 65, 90, 100, 120, 140, 120, 100, 90, 50, 20) + 290,
    y = c(0, 5, 30, 40, 40, 35, 5, 0, 80, 100, 105, 95, 70) + 310
  )
  eye_right <- eye_left
  eye_right$x = 1000 - eye_right$x

  # mouth - mirrors itself ----
  mouth_left <- data.frame(
    x = c(500, 450, 430, 410, 390, 350, 320, 300, 320, 340, 350, 360, 390,
          420, 430, 460, 490, 500),
    y = c(180, 180, 190, 220, 200, 220, 230, 250, 250, 240, 250, 250, 230,
          250, 250, 220, 250, 250),
    id = 1:18
  )
  mouth_right <- mouth_left
  mouth_right$x = 1000 - mouth_right$x
  mouth_right <- mouth_right[order(mouth_right$id, decreasing = TRUE), ]
  mouth_right <- mouth_right[!mouth_right$x == 500, ]
  mouth <- rbind(mouth_left, mouth_right)
  rm(mouth_left, mouth_right)

  # candle ----
  candle <- data.frame(
    x = c(490, 500, 510, 500, 490, 480),
    y = c(180, 180, 190, 230, 240, 200) + 5
  )

  # add face to image ----
  p <- image + # pumpkin: eyes
    ggforce::geom_shape(eye_left,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.012, "native"),
                        color = "darkorange", fill = "yellow") +
    ggforce::geom_shape(eye_right,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.012, "native"),
                        color = "darkorange", fill = "yellow") +
    # pumpkin: mouth
    ggforce::geom_shape(mouth,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.003, "native"),
                        color = "darkorange", fill = "yellow")

  # add candle to image ----
  p <- p + # pumpkin: candle
    ggforce::geom_shape(candle,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.01, "native"), fill = "gold1",
                        color = "yellow", expand = grid::unit(0.5, 'mm')) +
    ggforce::geom_shape(candle,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.005, "native"), fill = "darkgoldenrod1",
                        color = "gold1", expand = grid::unit(-0.5, 'mm')) +
    ggforce::geom_shape(candle,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0, "native"), fill = "darkorange",
                        color = "darkgoldenrod1", expand = grid::unit(-1, 'mm'))

  # return image ----
  return(p)
}
