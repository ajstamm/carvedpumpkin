#' Carve a cat
#'
#'
#' @param image A ggplot object.

create_cat <- function(image) {
  # cat - on broom? or with hat?
  xplus <- 320
  yplus <- 160
  # legs ----
  leftfore <- data.frame(
    x = c( 0, 20, 30, 40, 50, 40, 10,  5) * 3 + xplus,
    y = c(20, 30, 30, 40, 30, 20, 20, 15) * 3 + yplus
  )
  rightfore <- data.frame(
    x = c(15, 30, 30, 40, 50, 40, 25) * 3 + xplus,
    y = c( 0, 20, 30, 40, 30, 20,  0) * 3 + yplus
  )
  hind <- data.frame(
    x = c(35, 60, 60, 70, 80, 80, 70, 65, 35) * 3 + xplus,
    y = c( 5, 10, 20, 25, 20, 10, 10,  0,  0) * 3 + yplus
  )
  # tail ----
  tail <- data.frame(
    x = c(90, 90, 85, 90, 100, 100, 90, 100, 100, 95) * 3 + xplus,
    y = c(20, 40, 60, 80,  80,  70, 65,  40,  30, 20) * 3 + yplus
  )
  # head ----
  head <- data.frame(
    x = c(15, 20, 25, 30, 35, 35, 45, 50, 50, 20) * 3 + xplus,
    y = c(60, 60, 70, 75, 75, 85, 80, 70, 50, 50) * 3 + yplus
  )
  # body ----
  body <- data.frame(
    x = c(30, 50, 80, 90, 95, 90, 70, 30) * 3 + xplus,
    y = c(50, 55, 40, 30, 20, 10, 10, 30) * 3 + yplus
  )
  # candle ----
  candle <- data.frame(
    x = c(50, 45, 45, 50, 50, 55, 50, 55) * 3 + xplus,
    y = c(20, 25, 30, 35, 40, 35, 30, 25) * 3 + yplus
  )

  # draw image ----
  p <- image + # pumpkin: cat body
    ggforce::geom_shape(head,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.003, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_shape(tail,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.005, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_shape(body,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.02, "native"),
                        color = "yellow", fill = "yellow") +
    # pumpkin: cat legs
    ggforce::geom_shape(leftfore,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.008, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_shape(rightfore,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.01, "native"),
                        color = "yellow", fill = "yellow") +
    ggforce::geom_shape(hind,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.01, "native"),
                        color = "yellow", fill = "yellow")

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
