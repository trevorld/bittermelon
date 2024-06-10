#' Gray a bitmap
#'
#' `bm_gray()` grays a bitmap. `bm_grey()` is offered as an alias.
#'
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @examples
#' corn <- farming_crops_16x16()$corn$portrait
#' corn_gray <- bm_gray(corn)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_gray, compress = "v")
#' }
#' @export
bm_gray <- function(x) {
    UseMethod("bm_gray")
}

#' @rdname bm_gray
#' @export
bm_grey <- function(x) {
    UseMethod("bm_gray")
}

#' @rdname bm_gray
#' @export
bm_gray.bm_bitmap <- function(x) {
    x
}

#' @rdname bm_gray
#' @export
bm_gray.bm_list <- function(x) {
    bm_lapply(x, bm_gray)
}

#' @rdname bm_gray
#' @export
bm_gray.bm_pixmap <- function(x) {
    hex <- as.character(x)
    grey <- hex2grey(hex)
    alpha <- hex2alpha(hex)
    m <- matrix(grDevices::rgb(grey, grey, grey, alpha = alpha),
                nrow = nrow(x), ncol = ncol(x))
    as_bm_pixmap.matrix(m)
}

#' @rdname bm_gray
#' @export
`bm_gray.magick-image` <- function(x) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    magick::image_convert(x, colorspace = "Gray")
}

#' @rdname bm_gray
#' @export
bm_gray.nativeRaster <- function(x) {
    as.raster(bm_gray(as_bm_pixmap(x)), native = TRUE)
}

#' @rdname bm_gray
#' @export
bm_gray.raster <- function(x) {
    as.raster(bm_gray(as_bm_pixmap(x)))
}
