#' Plot bitmap object
#'
#' `plot.bm_bitmap()` plots a bitmap object to the graphics device.
#' It is a wrapper around `grid::grid.raster()` and `as.raster.bm_bitmap()`
#' which converts a bitmap glyph object to a raster object.
#'
#' @inheritParams format.bm_bitmap
#' @param ... Passed to [grid::grid.raster()].
#' @param col Character vector of R color specifications.
#' @param interpolate Passed to [grid::grid.raster()].
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   capital_r <- bm_extend(font[[str2ucp("R")]], left = 1L)
#'   capital_r <- bm_extend(capital_r, sides = 1L, value = 2L)  # add a border effect
#'
#'   plot(capital_r)
#'
#'   grid::grid.newpage()
#'   plot(capital_r, col = c("yellow", "blue", "red"))
#' @export
#' @return A `grid` rastergrob grob object silently.
#'         As a side effect will draw to graphics device.
#' @seealso [bm_bitmap()], [as.raster.bm_bitmap()]
plot.bm_bitmap <- function(x, ..., col = c("grey80", "black", "grey40"),
                          interpolate = FALSE) {
    grid::grid.raster(as.raster.bm_bitmap(x, col = col),
                      ..., interpolate = interpolate)
}

#' @rdname plot.bm_bitmap
#' @importFrom grDevices as.raster
#' @export
as.raster.bm_bitmap <- function(x, ..., col = c("grey80", "black", "grey40")) { # nolint
    x <- as.matrix(as_bm_bitmap(x))
    x <- x[rev(seq_len(nrow(x))), ]
    r <- apply(x, 2, function(i) col[i + 1L])
    grDevices::as.raster(r)
}
