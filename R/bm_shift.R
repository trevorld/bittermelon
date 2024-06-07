#' Shift elements within bitmaps
#'
#' Shifts non-padding elements within bitmaps by trimming on a specified side and padding on the other
#' while preserving the width and height of the original bitmap.
#'
#' This function is a convenience wrapper around [bm_trim()] and [bm_extend()].
#'
#' @inheritParams bm_extend
#' @param top Number of pixels to shift towards the top side.
#' @param right Number of pixels to shift towards the right side.
#' @param bottom Number of pixels to shift towards the bottom side.
#' @param left Number of pixels to shift towards the left side.
#' @inherit bm_clamp return
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(capital_r)
#' capital_r <- bm_shift(capital_r, bottom = 2L, right = 1L)
#' print(capital_r)
#' corn <- farming_crops_16x16()$corn$portrait
#' print(bm_padding_lengths(corn))
#' corn_shifted <- bm_shift(corn, left = 1L, top = 2L)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_shifted, bg = "cyan", compress = "v")
#' }
#' @seealso [bm_trim()] and [bm_extend()]
#' @export
bm_shift <- function(x, value,
                     top = NULL, right = NULL, bottom = NULL, left = NULL) {
    stopifnot(is.null(top) || is.null(bottom))
    stopifnot(is.null(left) || is.null(right))
    UseMethod("bm_shift")
}

#' @rdname bm_shift
#' @export
bm_shift.bm_bitmap <- function(x, value = 0L,
                               top = NULL, right = NULL, bottom = NULL, left = NULL) {
    bm_shift_bitmap(x, value = value,
                    top = top, right = right, bottom = bottom, left = left)
}

#' @rdname bm_shift
#' @export
bm_shift.bm_list <- function(x, ...) {
    bm_lapply(x, bm_shift, ...)
}

#' @rdname bm_shift
#' @export
bm_shift.bm_pixmap <- function(x, value = col2hex("transparent"),
                               top = NULL, right = NULL, bottom = NULL, left = NULL) {
    bm_shift_bitmap(x, value = value,
                    top = top, right = right, bottom = bottom, left = left)
}

#' @rdname bm_shift
#' @export
`bm_shift.magick-image` <- function(x, value = "transparent",
                               top = NULL, right = NULL, bottom = NULL, left = NULL) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    pm <- bm_shift_bitmap(as_bm_pixmap(x), value = col2hex(value),
                          top = top, right = right, bottom = bottom, left = left)
    magick::image_read(pm)
}

#' @rdname bm_shift
#' @export
bm_shift.nativeRaster <- function(x, value = col2int("transparent"),
                               top = NULL, right = NULL, bottom = NULL, left = NULL) {
    pm <- bm_shift_bitmap(as_bm_pixmap(x), value = int2col(as_native(value)),
                          top = top, right = right, bottom = bottom, left = left)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_shift
#' @export
bm_shift.raster <- function(x, value = "transparent",
                               top = NULL, right = NULL, bottom = NULL, left = NULL) {
    pm <- bm_shift_bitmap(as_bm_pixmap(x), value = col2hex(value),
                          top = top, right = right, bottom = bottom, left = left)
    as.raster(pm)
}

bm_shift_bitmap <- function(x, value = 0L,
                     top = NULL, right = NULL, bottom = NULL, left = NULL) {
    x <- bm_trim(x, top = top, right = right, bottom = bottom, left = left)
    x <- bm_extend(x, value = value, top = bottom, right = left, bottom = top, left = right)
    x
}
