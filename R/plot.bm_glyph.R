#' Plot bitmap glyph object
#'
#' `plot.bm_glyph()` plots a bitmap glyph object to the graphics device.
#'
#' @inheritParams as.character.bm_glyph
#' @param ... Passed to [grid::grid.raster()].
#' @param col Character vector of R color specifications. Passed to [as.raster.bm_glyph()].
#' @param interpolate Passed to [grid::grid.raster()].
#' @examples
#'   plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'   plus_sign[5L, 3:7] <- 1L
#'   plus_sign[3:7, 5L] <- 1L
#'   plus_sign <- bm_glyph(plus_sign)
#'   plot(plus_sign)
#' @export
#' @return A `grid` rastergrob grob object silently.
#'         As a side effect will draw to graphics device.
#' @seealso [bm_glyph()], [as.raster.bm_glyph()]
plot.bm_glyph <- function(x, ..., col = c("grey80", "black", "grey40"),
                          interpolate = FALSE) {
    grid::grid.raster(as.raster.bm_glyph(x, col = col),
                      ..., interpolate = interpolate)
}
