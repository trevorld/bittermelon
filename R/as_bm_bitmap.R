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

#' @inheritParams as_bm_list
#' @rdname as_bm_bitmap
#' @param direction For horizontal binding either "left-to-right" (default) or its aliases "ltr" and "lr"
#'                  OR "right-to-left" or its aliases "rtl" and "rl".
#'                  For vertical binding either "top-to-bottom" (default) or its aliases "ttb" and "tb"
#'                  OR "bottom-to-top" or its aliases "btt" and "bt".
#'                  The `direction` argument is not case-sensitive.
#' @examples
#'   font_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
#'   font <- read_yaff(font_file)
#'   bm <- as_bm_bitmap("RSTATS", font = font)
#'   print(bm, px = px_ascii)
#'   bm <- as_bm_bitmap("RSTATS", direction = "top-to-bottom", font = font)
#'   print(bm, px = px_ascii)
#' @export
as_bm_bitmap.character <- function(x, ...,
                                   direction = "left-to-right",
                                   font = bm_font()) {
    bml <- as_bm_list(x, font = font)

    is_ltr <- c(tolower(direction) %in% c("left-to-right", "ltr", "lr"))
    is_rtl <- c(tolower(direction) %in% c("right-to-left", "rtl", "rl"))
    is_ttb <- c(tolower(direction) %in% c("top-to-bottom", "ttb", "tb"))
    is_bbt <- c(tolower(direction) %in% c("bottom-to-top", "bbt", "bt"))
    stopifnot(is_ltr || is_rtl || is_ttb || is_bbt)
    if (is_ltr || is_rtl)
        bm <- bm_call(bml, cbind, direction = direction)
    else
        bm <- bm_call(bml, rbind, direction = direction)
    bm
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
