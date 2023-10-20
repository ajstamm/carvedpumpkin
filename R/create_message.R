#' Write the message
#'
#' @description
#' Write your orange message in the sky.
#'
#' @param image A ggplot object.
#' @param note  A text string.
#'
#'
#'

create_message <- function(image, note = "Boo") {
  p <- image +
    ggplot2::annotate("text", x = 498, y = 602, label = note,
                      color = "chocolate3", size = 10, family = "serif") +
    ggplot2::annotate("text", x = 499, y = 601, label = note,
                      color = "chocolate2", size = 10, family = "serif") +
    ggplot2::annotate("text", x = 500, y = 600, label = note,
                      color = "chocolate1", size = 10, family = "serif")
  return(p)
}
