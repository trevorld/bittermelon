library("bittermelon")
library("grid")
library("piecepackr")
library("jpeg")

font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
bitter <- as_bm_list("bittermelon", font = font) |> bm_call(cbind)
# image source: https://ac-illust.com/tw/clip-art/100441/%E8%8B%A6%E7%93%9C
# Permissions:
#  * 可免費用於商業用途 (commercial use allowed)
#  * 無需註明出處 (attribution not required)
melon <- jpeg::readJPEG("raw-data/100441.jpg")

draw_logo <- function() {
    hex <- pp_shape("convex6")
    grid.newpage()
    grid.draw(hex$shape(gp = gpar(col = NA, fill = "white")))
    clipping <- polygonGrob(x = c(0.4, 0.2, 0.5, 0.75, 0.75, 0.5),
                            y = c(0.1, 0.5, 0.95, 0.95, 0.6, 0.1))
    grid.raster(melon, vp = viewport(x = 0.57, y = 0.34, height = 0.75, clip = clipping))
    plot(bitter, col = c("transparent", "black"),
         vp = viewport(y = 0.70, width=0.75, height = 0.21))
    grid.draw(hex$mat(mat_width = 0.03, gp = gpar(col = NA, fill = "black")))
}

w <- 3.0
svg("man/figures/logo.svg", width = w, height = w, bg = "transparent")
draw_logo()
dev.off()

png("man/figures/logo.png", width = w, height = w, units = "in",
    res = 72, bg = "transparent")
draw_logo()
dev.off()
