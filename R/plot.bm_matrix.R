#' Plot bitmap/pixmap objects
#'
#' `plot.bm_bitmap()` plots a [bm_bitmap()] object to the graphics device
#' while `plot.bm_pixmap()` plots a [bm_pixmap()] object to the graphics device.
#' They are wrappers around [grid::grid.raster()] and `as.raster.bm_bitmap()`
#' or `as.raster.bm_pixmap()`.
#' which converts a bitmap glyph object to a raster object.
#' `col_bitmap` is a builtin color string vectors intended for use with the `col`
#' argument for casting [bm_bitmap()] objects to pixmap objects.
#'
#' @inheritParams format.bm_bitmap
#' @param ... Passed to [grid::grid.raster()].
#' @param col Character vector of R color specifications.
#'            First color is used for values equal to 0, second color for values equal to 1, etc.
#' @param interpolate Passed to [grid::grid.raster()].
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- bm_extend(font[[str2ucp("R")]], left = 1L)
#' capital_r <- bm_extend(capital_r, sides = 1L, value = 2L)  # add a border effect
#'
#' plot(capital_r)
#'
#' plot(capital_r, col = c("yellow", "blue", "red"))
#'
#' crops <- farming_crops_16x16()
#' grapes <- crops$grapes$portrait
#' plot(grapes)
#' @return `plot.bm_bitmap()` and `plot.bm_pixmap()` return a [grid::rasterGrob()] object silently.
#'         As a side effect will draw to graphics device.
#'         `as.raster.bm_bitmap()` and `as.raster.bm_pixmap()` return "raster" objects (see [grDevices::as.raster()]).
#' @seealso [bm_bitmap()], [bm_pixmap()]
#' @name plot.bm_matrix
#' @export
plot.bm_bitmap <- function(x, ...,
                           col = getOption("bittermelon.col", col_bitmap),
                           interpolate = FALSE) {
    grid::grid.newpage()
    grid::grid.raster(as.raster.bm_bitmap(x, col = col),
                      ..., interpolate = interpolate)
}

#' @rdname plot.bm_matrix
#' @export
plot.bm_pixmap <- function(x, ...,
                          interpolate = FALSE) {
    grid::grid.newpage()
    grid::grid.raster(as.raster.bm_pixmap(x),
                      ..., interpolate = interpolate)
}

#' @rdname plot.bm_matrix
#' @importFrom grDevices as.raster
#' @param native If `TRUE` return a "nativeRaster" object instead of a "raster" object.
#'               This will require that the suggested package [`farver`][farver::farver] is installed.
#' @export
as.raster.bm_bitmap <- function(x, native = FALSE, ...,
                                col = getOption("bittermelon.col", col_bitmap)) { # nolint
    if (native) {
        as_native_raster.bm_bitmap(x, col = col)
    } else {
        if (nrow(x) > 0L && ncol(x) > 0L) {
            cols <- col[as.integer(as.matrix(x, first_row_is_top = TRUE)) + 1L]
            m <- matrix(cols, nrow = nrow(x), ncol = ncol(x))
            as.raster(m)
        } else {
            as.raster(matrix(character(0L), nrow = nrow(x), ncol = ncol(x)))
        }
    }
}

as_native_raster.bm_bitmap <- function(x, col = getOption("bittermelon.col", col_bitmap)) {
    if (nrow(x) > 0L && ncol(x) > 0L) {
        x <- as.matrix(x, first_row_is_top = TRUE)
        r <- apply(x, 2, function(i) col[i + 1L])
        cols <- col2int(as.character(t(r)))
        m <- matrix(cols, nrow = nrow(x), ncol = ncol(x))
    } else {
        m <- matrix(integer(0L), nrow = nrow(x), ncol = ncol(x))
    }
    class(m) <- "nativeRaster"
    m

}

#' @rdname plot.bm_matrix
#' @importFrom grDevices as.raster
#' @export
as.raster.bm_pixmap <- function(x, native = FALSE, ...) { # nolint
    if (native) {
        as_native_raster.bm_pixmap(x)
    } else {
        as.raster(as.matrix(x, first_row_is_top = TRUE))
    }
}

# nativeRaster is same dimension as raster
# if you cast to vector by **rows** (instead of **columns**)
# but then build a matrix from this vector by **columns** (instead of **rows**)
# nativeRaster uses integer for colors...

# Is there a S3 generic to convert to a native raster?
as_native_raster.bm_pixmap <- function(x, ...) {
    if (nrow(x) > 0L && ncol(x) > 0L) {
        x <- as.matrix(x, first_row_is_top = TRUE)
        cols <- col2int(as.character(t(x)))
        m <- matrix(cols, nrow = nrow(x), ncol = ncol(x))
    } else {
        m <- matrix(integer(0L), nrow = nrow(x), ncol = ncol(x))
    }
    class(m) <- "nativeRaster"
    m
}

#' @rdname plot.bm_matrix
#' @export
col_bitmap <- c("transparent", "black", "grey50", "grey25")
