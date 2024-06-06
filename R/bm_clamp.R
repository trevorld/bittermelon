#' Clamp bitmap values
#'
#' `bm_clamp()` \dQuote{clamps} [bm_bitmap()] integers that lie outside an interval.
#' The default coerces a multiple-integer-valued bitmap
#' into a binary bitmap (as expected by most bitmap font formats).
#' For pixmap objects non-background pixels are all coerced to a single value.
#'
#' @param x Either a [bm_bitmap()], [bm_font()], [bm_list()], ["magick-image"][magick::image_read()], "nativeRaster", [bm_pixmap()], or ["raster"][grDevices::as.raster()]  object.
#' @param lower Integer value.  Any value below `lower` will be clamped.
#' @param upper Integer value.  Any value above `upper` will be clamped.
#' @param value Integer vector of length one or two of replacement value(s).
#'              If `value` is length one
#'              any values above `upper` are replaced by `value`
#'              while those below `lower` are replaced by `lower`.
#'              If `value` is length two any values above `upper`
#'              are replaced by `value[2]` and any values below `lower`
#'              are replaced by `value[1]`.
#'              For pixmap objects indicate requested non-background color.
#' @examples
#' plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#' plus_sign[5L, 3:7] <- 2L
#' plus_sign[3:7, 5L] <- 2L
#' plus_sign_glyph <- bm_bitmap(plus_sign)
#' print(plus_sign_glyph)
#'
#' plus_sign_clamped <- bm_clamp(plus_sign_glyph)
#' print(plus_sign_clamped)
#'
#' tulip <- farming_crops_16x16()$tulip$portrait
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 8L) {
#'   print(bm_clamp(tulip, "magenta"), compress = "v")
#' }
#' @return Depending on `x` either a [bm_bitmap()], [bm_font()], [bm_list()], [magick-image][magick::image_read()], "nativeRaster", [bm_pixmap()], or [raster][grDevices::as.raster()]  object.
#' @export
bm_clamp <- function(x, ...) {
    UseMethod("bm_clamp")
}

#' @rdname bm_clamp
#' @export
bm_clamp.bm_bitmap <- function(x, lower = 0L, upper = 1L, value = upper, ...) {
    stopifnot(length(value) > 0L, length(value) < 3L)
    if (length(value) == 2L) {
        value_lower <- value[2L]
        value_upper <- value[2L]
    } else {
        value_lower <- lower
        value_upper <- value
    }
    bm_clamp_bitmap(x, lower = lower, upper = upper,
                    value_lower = value_lower, value_upper = value_upper)
}

#' @rdname bm_clamp
#' @param ... Additional arguments to be passed to or from methods.
#' @export
bm_clamp.bm_list <- function(x, ...) {
    bm_lapply(x, bm_clamp, ...)
}

#' @rdname bm_clamp
#' @export
bm_clamp.bm_pixmap <- function(x, value = col2hex("black"), 
                               bg = col2hex("transparent"), ...) {
    value <- col2hex(value)
    bg <- col2hex(bg)
    w <- which(as.character(x) != bg)
    if (length(w))
        x[w] <- value
    x
}

#' @rdname bm_clamp
#' @export
`bm_clamp.magick-image` <- function(x, value = "black", 
                                    bg = "transparent", ...) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    value <- col2hex(value)
    bg <- col2hex(bg)
    pm <- bm_clamp(as_bm_pixmap(x), value = value, bg = bg)
    magick::image_read(pm)
}

#' @rdname bm_clamp
#' @param bg Bitmap background value.
#' @export
bm_clamp.nativeRaster <- function(x, value = col2int("black"), 
                                  bg = col2int("transparent"), ...) {
    value <- int2col(as_native(value))
    bg <- int2col(as_native(bg))
    pm <- bm_clamp(as_bm_pixmap(x), value = value, bg = bg)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_clamp
#' @export
bm_clamp.raster <- function(x, value = "black", 
                               bg = "transparent", ...) {
    value <- col2hex(value)
    bg <- col2hex(bg)
    pm <- bm_clamp(as_bm_pixmap(x), value = value, bg = bg)
    as.raster(pm)
}

bm_clamp_bitmap <- function(x, lower = 0L, upper = 1L,
                            value_lower = lower, value_upper = upper) {
    stopifnot(lower <= upper)
    indices <- which(as.logical(x < lower))
    if (length(indices)) {
        x[indices] <- value_lower
    }
    indices <- which(as.logical(x > upper))
    if (length(indices)) {
        x[indices] <- value_upper
    }
    x
}
