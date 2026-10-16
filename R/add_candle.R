#' Add a candle to the jack-o-lantern
#'
#' @param image           Plot object; background and vegetable.
#' @param vertical_offset Numeric; number of pixels to shift the candle height.
#'
#' This function adds a candle at a defined height to accommodate variation
#' in carving heights.
#'
#' @examples
#' image <- create_background() |> grow_pumpkin() |> carve_bat()
#' add_candle(image, vertical_offset = -20)
#'
#'
# offsets
# bat = -20
# ghost = -70
# cat = -20
# smile = -40
add_candle <- function(image, vertical_offset = 0) {
  # define candle ----
  candle <- data.frame(
    x = c(49, 50, 51, 50, 49, 48) * 10,
    y = c(19, 16, 20, 25, 27, 23) * 10 + vertical_offset
  )

  # define colors ----
  my_pal <- c("orangered", "darkorange1", "darkgoldenrod1", "yellow")
  my_gradient <- grid::radialGradient(my_pal,
                                      cx1 = grid::unit(.5, "npc"),
                                      cx2 = grid::unit(.5, "npc"),
                                      cy1 = grid::unit(0, "npc"),
                                      cy2 = grid::unit(.4, "npc"),
                                      r2 = grid::unit(.5, "npc"),
                                      extend = "none")

  # add candle to image ----
  p <- image +
    ggforce::geom_shape(candle,
                        mapping = ggplot2::aes(x = !!dplyr::sym("x"),
                                               y = !!dplyr::sym("y")),
                        radius = grid::unit(0.01, "native"),
                        color = "transparent", fill = my_gradient,
                        expand = grid::unit(0.1, 'mm'))

  # return image ----
  return(p)
}
