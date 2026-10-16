#' Draw the backdrop
#'
#' @param time   String; time of day. Accepts "day", "dawn", "dusk", and
#'               "night".
#' @param season String; season of the year (US). Accepts "spring", "summer",
#'               "autumn", and "winter".
#'
#' This function draws a background for the image based on the options chosen.
#'
#' @examples
#' create_background(time = "day", season = "winter")
#'


create_background <- function(time = "night", season = "autumn") {
  # plot settings ----
  graphics::par(bg = "black", mar = c(0, 0, 0, 0), mai = c(0, 0, 0, 0))

  # backdrop ----
  ## define colors ----
  seasons <- list(spring = "olivedrab2", summer = "green4",
                  autumn = "darkolivegreen4", winter = "snow")
  times <- list(day = c("skyblue1", "lightskyblue1"),
                night = c("blue4", "purple4"),
                dawn = c("orangered", "yellow"),
                dusk = c("firebrick2", "darkorchid4"))
  my_pal <- unlist(c(seasons[season], times[time]))

  # sky_gradient <- grDevices::colorRampPalette(my_pal)(100)
  # bg <- tibble::tibble(x = (0:100) * 10, y = (0:100) * 7)

  my_gradient <- grid::linearGradient(my_pal,
                                      x1 = grid::unit(.5, "npc"),
                                      x2 = grid::unit(.5, "npc"),
                                      y1 = grid::unit(0, "npc"),
                                      y2 = grid::unit(1, "npc"))

  ## draw backdrop ----
  p <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::lims(x = c(0, 1000), y = c(0, 700)) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = "black"),
      panel.background = ggplot2::element_rect(fill = my_gradient),
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      legend.position = "none")

  # stars ----
  if (time == "day") {
    # define sun ----
    my_sun <- grid::radialGradient(c("yellow", "lightgoldenrod1",
                                     "transparent"),
                                   cx1 = grid::unit(.5, "npc"),
                                   cx2 = grid::unit(.5, "npc"),
                                   cy1 = grid::unit(.5, "npc"),
                                   cy2 = grid::unit(.5, "npc"),
                                   r2  = grid::unit(.5, "npc"))
    sun_df <- data.frame(x = 1000, y = 700)
    # draw sun ----
    p <- p + ggplot2::geom_point(data = sun_df,
                                 ggplot2::aes(x = !!dplyr::sym("x"),
                                              y = !!dplyr::sym("y")),
                                 size = 50, shape = 21, color = "transparent",
                                 fill = my_sun)
  } else if (time %in% c("dawn", "dusk")) {
    # draw sun ----
    # doesn't look right - keep testing
    # my_sun <- grid::radialGradient(c("orangered", "darkorange", # "gold1",
    #                                  "transparent"),
    #                                cx1 = grid::unit(0, "npc"),
    #                                cx2 = grid::unit(0, "npc"),
    #                                cy1 = grid::unit(0, "npc"),
    #                                cy2 = grid::unit(0, "npc"),
    #                                r2  = grid::unit(1, "npc"))
    # sun_df <- data.frame(x = 1, y = 300)
    # p <- p + ggforce::geom_arc_bar(data = sun_df,
    #                                ggplot2::aes(x0 = x, y0 = y,
    #                                             start = 0, end = 1.57,
    #                                             r0 = 0, r = 300),
    #                                fill = my_sun, color = "transparent")
    ## draw stars ----
    star_num <- 50
    stars <- data.frame(x = sample(seq(0, 1000), star_num, replace = TRUE),
                        y = sample(seq(500, 700), star_num, replace = TRUE))
    p <- p + ggplot2::geom_point(data = stars, colour = "white",
                                 ggplot2::aes(x = !!dplyr::sym("x"),
                                              y = !!dplyr::sym("y")),
                                 size = 1, shape = ".", alpha = 1)
  } else {
    # define stars ----
    star_num <- 200
    stars <- data.frame(x = sample(seq(0, 1000), star_num, replace = TRUE),
                        y = sample(seq(300, 700), star_num, replace = TRUE))
    ## draw stars ----
    p <- p + ggplot2::geom_point(data = stars, colour = "white",
                                 ggplot2::aes(x = !!dplyr::sym("x"),
                                              y = !!dplyr::sym("y")),
                                 size = 1, shape = ".", alpha = 1)
  }
  # return ----
  return(p)
}

