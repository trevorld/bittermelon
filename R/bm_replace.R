#' Replace a color with another color
#'
#' `bm_replace()` replaces a bitmap color with another color.
#' In particular default arguments will try to replace the background color.
#' @param x A bitmap object.
#' @param value New bitmap color value.
#' @param old Old bitmap color value to replace.
#' @return A bitmap object.
#' @examples
#' corn <- farming_crops_16x16()$corn$portrait
#' bm_replace(corn, "cyan")
#' bm_replace(corn, "magenta")
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
#' @param ... Passed to `bm_replace.bm_bitmap()`.
#' @export
bm_replace.bm_list <- function(x, value = 0L, ...) {
    bm_lapply(x, bm_replace, value = value, ...)
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
