#' Carve your jack-o-lantern
#'
#' @param message   String; the message you want to display on your image.
#' @param carving   String; the image to carve on your jack-o-lantern.
#'                  Accepts "smile", "bat", "ghost", and "cat".
#' @param time      String; time of day. Accepts "day", "dawn", "dusk", and
#'                  "night".
#' @param season    String; season of the year (US). Accepts "spring", "summer",
#'                  "autumn", and "winter".
#' @param vegetable String; vegetable to carve. Accepts "pumpkin" and "turnip".
#' @param font_size Numeric; message font size.
#'
#' This function draws a jack-o-lantern for you based on the settings you
#' choose.
#'
#' @examples
#'
#' draw_jackolantern(carving = "smile", time = "night", season = "autumn",
#'                   message = "This is a test!", vegetable = "turnip")
#'
#' @export
#'


draw_jackolantern <- function(message = "Happy Autumn!", carving = "face",
                              time = "night", season = "autumn",
                              vegetable = "pumpkin", font_size = 10) {
  # create background ----
  image <- create_background(time = time, season = season)
  image <- write_message(image, note = message, time = time,
                         font_size = font_size)
  # grow pumpkin ----
  if (vegetable == "pumpkin") {
    image <- grow_pumpkin(image)
  } else if (vegetable == "turnip") {
    image <- grow_turnip(image)
  }
  # carve pumpkin ----
  if (carving == "smile") {
    image <- carve_smile(image)  |> add_candle(vertical_offset = -40)
  } else if (carving == "cat")   {
    image <- carve_cat(image)   |> add_candle(vertical_offset = -20)
  } else if (carving == "ghost") {
    image <- carve_ghost(image) |> add_candle(vertical_offset = -70)
  } else if (carving == "bat")   {
    image <- carve_bat(image)   |> add_candle(vertical_offset = -20)
  }
  # return image ----
  return(image)
}
