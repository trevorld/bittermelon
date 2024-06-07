#' Replace a color in a bitmap with another color
#'
#' `bm_replace()` replaces a bitmap color with another color.
#' In particular default arguments will try to replace the background color.
#' @inheritParams bm_clamp
#' @param value New bitmap \dQuote{color} value.
#' @param old Old bitmap \dQuote{color} value to replace.
#' @inherit bm_clamp return
#' @examples
#' corn <- farming_crops_16x16()$corn$portrait
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(bm_replace(corn, "cyan"), compress = "v")
#' }
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(bm_replace(capital_r, 2L))
#' @export
bm_replace <- function(x, value, old) {
    UseMethod("bm_replace")
}

#' @rdname bm_replace
#' @export
bm_replace.bm_bitmap <- function(x, value = 0L, old = x[1L, 1L]) {
    w_old <- which(as.logical(x == old))
    if (length(w_old))
        x[w_old] <- value
    x
}

#' @rdname bm_replace
#' @export
bm_replace.bm_list <- function(x, ...) {
    bm_lapply(x, bm_replace, ...)
}

#' @rdname bm_replace
#' @export
bm_replace.bm_pixmap <- function(x, value = col2hex("transparent"), old = x[1L, 1L]) {
    value <- col2hex(value)
    old <- col2hex(old)
    w_old <- which(as.logical(x == old))
    if (length(w_old))
        x[w_old] <- value
    x
}

#' @rdname bm_replace
#' @export
`bm_replace.magick-image` <- function(x, value = "transparent", old = as.raster(x)[1L, 1L]) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    value <- col2hex(value)
    old <- col2hex(old)
    x <- as_bm_pixmap(x)
    w_old <- which(as.logical(x == old))
    if (length(w_old))
        x[w_old] <- value
    magick::image_read(x)
}

#' @rdname bm_replace
#' @export
bm_replace.nativeRaster <- function(x, value = col2int("transparent"), old = x[1L, 1L]) {
    value <- int2col(as_native(value))
    old <- int2col(as_native(old))
    x <- as_bm_pixmap(x)
    w_old <- which(as.logical(x == old))
    if (length(w_old))
        x[w_old] <- value
    as.raster(x, native = TRUE)
}

#' @rdname bm_replace
#' @export
bm_replace.raster <- function(x, value = "transparent", old = x[1L, 1L]) {
    value <- col2hex(value)
    old <- col2hex(old)
    x <- as_bm_pixmap(x)
    w_old <- which(as.logical(x == old))
    if (length(w_old))
        x[w_old] <- value
    as.raster(x)
}
