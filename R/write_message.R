#' Write the message
#'
#' @description
#' Write your orange message in the sky.
#'
#' @param image     Plot object; backdrop for the image.
#' @param note      String; message to place on the image.
#' @param font_size Numeric; message font size.
#' @param time      String; time of day. Accepts "day", "dawn", "dusk", and
#'                  "night".
#'
#' @examples
#' create_background() |> write_message("Hello!")
#'
#'
#'

write_message <- function(image, note = "Boo", time = "day", font_size = 10) {
  pals <- list(
    night = list(upper = "purple4", lower = "blue4", main = "steelblue1"),
    day   = list(upper = "deepskyblue1", lower = "dodgerblue1", main = "navy"),
    dawn  = list(upper = "firebrick1", lower = "red", main = "red4"),
    dusk  = list(upper = "purple4", lower = "magenta4", main = "orchid1")
  )
  pal <- pals[[time]]

  p <- image +
    ggplot2::annotate("text", x = 498, y = 602, label = note,
                      color = pal$upper, size = font_size, family = "serif") +
    ggplot2::annotate("text", x = 500, y = 600, label = note,
                      color = pal$lower, size = font_size, family = "serif") +
    ggplot2::annotate("text", x = 499, y = 601, label = note,
                      color = pal$main, size = font_size, family = "serif")
  return(p)
}
