#' Coerce to bitmap glyph objects
#'
#' `as_bm_bitmap()` turns an existing object into a `bm_bitmap()` object.
#'
#' @param x An object that can reasonably be coerced to a `bm_bitmap()` object.
#' @param ... Further arguments passed to or from other methods.
#' @return A `bm_bitmap()` object.
#' @examples
#'  space_matrix <- matrix(0L, nrow = 16L, ncol = 16L)
#'  space_glyph <- as_bm_bitmap(space_matrix)
#'  is_bm_bitmap(space_glyph)
#' @seealso [bm_bitmap()]
#' @export
as_bm_bitmap <- function(x, ...) {
    UseMethod("as_bm_bitmap")
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.matrix <- function(x, ...) {
    if (!is.integer(x)) {
        x[, ] <- suppressWarnings(as.integer(x))
    }
    stopifnot(!any(is.na(x)))
    class(x) <- c("bm_bitmap", class(x))
    x
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.default <- function(x, ...) {
    as_bm_bitmap.matrix(as.matrix(x))
}

#' @rdname as_bm_bitmap
#' @param width Desired width of bitmap
#' @param height Desired height of bitmap
#' @param png_device A function taking arguments `filename`, `width`, and `height`
#'                   that starts a graphics device that saves a png image
#'                   with a transparent background.  By default will use [ragg::agg_png()]
#'                   if available else the \dQuote{cairo} version of [grDevices::png()]
#'                   if available else just [grDevices::png()].
#' @param threshold  If any png channel weakly exceeds this threshold
#'                   (on an interval from zero to one)
#'                   then the pixel is determined to be \dQuote{black}.
#' @examples
#'
#'  if (require("grid") && capabilities("png")) {
#'    circle <- as_bm_bitmap(circleGrob(r = 0.25), width = 16L, height = 16L)
#'    print(circle, px = c(".", "@"))
#'
#'    inverted_exclamation <- as_bm_bitmap(textGrob("!", rot = 180),
#'                                         width = 8L, height = 16L)
#'    print(inverted_exclamation, px = c(".", "@"))
#'  }
#' @importFrom grid gpar grob grid.draw pushViewport popViewport viewport
#' @export
as_bm_bitmap.grob <- function(x, ..., width = 8L, height = 16L,
                              png_device = NULL, threshold = 0.25) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1L)
        on.exit(grDevices::dev.set(current_dev))

    png_file <- tempfile(fileext = ".png")
    on.exit(unlink(png_file))

    if (is.null(png_device))
        png_device <- default_png_device()

    png_device(filename = png_file, height = height, width = width)
    pushViewport(viewport(gp = gpar(lwd = 0, col = "black", fill = "black")))
    grid.draw(x)
    popViewport()
    grDevices::dev.off()

    array_glyph <- png::readPNG(png_file, native = FALSE)
    m_bitmap <- apply(array_glyph, c(1, 2), function(x) as.integer(any(x >= threshold)))
    bm_bitmap(m_bitmap[rev(seq.int(nrow(m_bitmap))), ])
}

default_png_device <- function() {
    if (requireNamespace("ragg", quietly = TRUE)) {
        function(filename, width, height)
            ragg::agg_png(filename, width, height, background = "transparent")
    } else if (capabilities("png") && capabilities("cairo")) {
        function(filename, width, height)
            grDevices::png(filename, width, height, bg = "transparent", type = "cairo")
    } else {
        stopifnot(capabilities("png"))
        function(filename, width, height)
            grDevices::png(filename, width, height, bg = "transparent")
    }
}
