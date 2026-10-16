# Carve a jack-o-lantern in R

Happy Halloween 2026!

I made this package back in 2023 partly for fun (who doesn't want to draw a jack-o-lantern in R?) and partly as a simple example of how to write a package.

To use the package, install it, then run the only function that is exported, `draw_jackolantern()`. For 2026, I added some fun options for carvings, veggies, and time settings.

*Warning:*  
This package uses gradients in the `grid` package, which may not work with default settings on Windows computers. To fix this, in Global Options (in the Tools menu), select "General" > "Graphics". Change the "Backend" option to "Cairo PNG".

Feel free to download and use this package. Let me know how it works for you (or doesn't) by posting an issue. 

