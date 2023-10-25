# Carve a jack-o-lantern in R

Happy Halloween 2023!

I made this package partly for fun (who doesn't want to draw a jack-o-lantern in R?) and partly as a simple example of how to write a package. I added the face image to my drawing last year (2022) and cleaned it up this year (2023) when I converted my drawing to a package. I also added a cat this year, so your pumpkins can have some variety. Maybe I'll add something else next year.

To use the package, install it, then run the only function that is exported, `draw_jackolantern()`.

Warning: it's graphic-heavy and not optimized, so it may take several seconds to run. I suggest saving the image first with `p <- draw_jackolantern()`, which should run quickly, then running `print(p)` separately, since the drawing part is what takes a little while and occasionally, only half the image will be drawn. Eventually, I may fix that.

Feel free to download and use this package. Let me know how it works for you (or doesn't) by posting an issue. 

