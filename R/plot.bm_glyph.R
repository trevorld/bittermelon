#' Plot bitmap glyph object
#'
#' `plot.bm_glyph()` plots a bitmap glyph object to the graphics device.
#'
#' @inheritParams as.character.bm_glyph
#' @param ... Passed to [grid::grid.raster()].
#' @param col Character vector of R color specifications. Passed to [as.raster.bm_glyph()].
#' @param interpolate Passed to [grid::grid.raster()].
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   capital_r <- font[[code_point_from_char("R")]]
#'
#'   plot(capital_r)
#'
#'   grid::grid.newpage()
#'   plot(capital_r, col = c("yellow", "blue"))
#' @export
#' @return A `grid` rastergrob grob object silently.
#'         As a side effect will draw to graphics device.
#' @seealso [bm_glyph()], [as.raster.bm_glyph()]
plot.bm_glyph <- function(x, ..., col = c("grey80", "black", "grey40"),
                          interpolate = FALSE) {
    grid::grid.raster(as.raster.bm_glyph(x, col = col),
                      ..., interpolate = interpolate)
}
