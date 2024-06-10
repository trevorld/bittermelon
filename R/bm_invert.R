#' Invert (negate) a bitmap
#'
#' `bm_invert()` inverts (negates) a bitmap.
#'
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- as_bm_bitmap("R", font = font)
#' capital_r_inverted <- bm_invert(capital_r)
#' print(capital_r_inverted)
#' 
#' corn <- farming_crops_16x16()$corn$portrait
#' corn_inverted <- bm_invert(corn)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_inverted, compress = "v", bg = "black")
#' }
#' @export
bm_invert <- function(x) {
    UseMethod("bm_invert")
}

#' @rdname bm_invert
#' @export
bm_invert.bm_bitmap <- function(x) {
    !x
}

#' @rdname bm_invert
#' @export
bm_invert.bm_list <- function(x) {
    bm_lapply(x, bm_invert)
}

#' @rdname bm_invert
#' @export
bm_invert.bm_pixmap <- function(x) {
    hex <- as.character(x)
    red <- hex2red(hex)
    green <- hex2green(hex)
    blue <- hex2blue(hex)
    alpha <- hex2alpha(hex)
    m <- matrix(grDevices::rgb(1 - red, 1 - green, 1 - blue, alpha = alpha),
                nrow = nrow(x), ncol = ncol(x))
    as_bm_pixmap.matrix(m)
}

#' @rdname bm_invert
#' @export
`bm_invert.magick-image` <- function(x) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    magick::image_negate(x)
}

#' @rdname bm_invert
#' @export
bm_invert.nativeRaster <- function(x) {
    as.raster(bm_invert(as_bm_pixmap(x)), native = TRUE)
}

#' @rdname bm_invert
#' @export
bm_invert.raster <- function(x) {
    as.raster(bm_invert(as_bm_pixmap(x)))
}
