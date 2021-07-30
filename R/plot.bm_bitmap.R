#' Plot bitmap object
#'
#' `plot.bm_bitmap()` plots a bitmap object to the graphics device.
#'
#' @inheritParams as.character.bm_bitmap
#' @param ... Passed to [grid::grid.raster()].
#' @param col Character vector of R color specifications. Passed to [as.raster.bm_bitmap()].
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
#' @seealso [bm_bitmap()], [as.raster.bm_bitmap()]
plot.bm_bitmap <- function(x, ..., col = c("grey80", "black", "grey40"),
                          interpolate = FALSE) {
    grid::grid.raster(as.raster.bm_bitmap(x, col = col),
                      ..., interpolate = interpolate)
}
