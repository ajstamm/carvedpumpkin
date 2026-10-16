#' Carve a ghost
#'
#' @param image     Plot object; backdrop for the image.
#' @param vegetable String; vegetable to carve. Accepts "pumpkin" and "turnip".
#'
#' This function carves a ghost in the jack-o-lantern.
#'
#' @examples
#' image <- create_background() |> grow_pumpkin()
#' carve_ghost(image)
#'

carve_ghost <- function(image, vegetable = "pumpkin") {
  # bounds: x = 300:700, y = 180:250
  vertical_offset <- -40
  cols <- list(
    pumpkin = "goldenrod1",
    turnip = "lavenderblush3"
  )
  my_col = cols[[vegetable]]

  # body - mirror ----
  body_left <- data.frame(
    x = c(12,  5,  1,  0,  1,  4,  5,  7, 10, 11, 11,  9,  9,  8,  9, 13, 16) *
      10 + 335,
    y = c(23, 25, 25, 24, 22, 21, 18, 17, 15, 13, 11,  7,  3,  1,  0,  2,  0) *
      10 + 150 + vertical_offset,
    id = 1:17 # number of points
  )
  body_right <- body_left
  body_right$x = 1000 - body_right$x
  body_right <- body_right[order(body_right$id, decreasing = TRUE), ]
  body_right <- body_right[!body_right$x == 500, ]
  body <- rbind(body_left, body_right)
  rm(body_left, body_right)

  # add ghost to image ----
  p <- image +
    ggforce::geom_shape(body,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.003, "native"),
                        color = my_col, fill = my_col) +
    ggforce::geom_ellipse(color = my_col, fill = my_col,
                          ggplot2::aes(x0 = 500, y0 = 400 + vertical_offset,
                                       a = 30, b = 50, angle = 0))

  # return image ----
  return(p)
}
