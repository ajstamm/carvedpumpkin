#' Carve your jack-o-lantern
#'
#' @param message A text string.
#' @param shape   The image to carve on your pumpkin. Currently accepts "face"
#'                and "cat".
#'
#' @examples
#'
#' pumpkin <- draw_jackolantern()
#' print(pumpkin)
#'
#'
#'
#' @export


draw_jackolantern <- function(message = "Happy Autumn!", shape = "face") {
  # draw pumpkin
  image <- create_pumpkin()
  image <- create_message(image, note = message)
  # pumpkin design ----
  if (shape == "face") {
    image <- create_face(image)
  } else if (shape == "cat") {
    image <- create_cat(image)
  }
  # return image ----
  return(image)
}
