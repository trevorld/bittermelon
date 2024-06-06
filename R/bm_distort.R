#' Resize images via distortion.
#'
#' `bm_distort()` resize images to arbitrary width and height value via [magick::image_resize()].
#'
#' @inheritParams as.raster.bm_bitmap
#' @inheritParams as_bm_bitmap.grob
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#'
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' dim(capital_r) # 8 x 16
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   print(bm_distort(capital_r, width = 9L, height = 21L))
#' }
#' @seealso [bm_expand()] for expanding width/height by integer multiples.
#'          [bm_resize()] for resizing an image via trimming/extending an image.
#' @export
bm_distort <- function(x, width = NULL, height = NULL, ...) {
    UseMethod("bm_distort")
}

#' @rdname bm_distort
#' @export
bm_distort.bm_bitmap <- function(x, width = NULL, height = NULL, ...,
                                 filter = "Point", threshold = 0.50) {
    pm <- bm_distort(as_bm_pixmap(x), width, height, filter = filter)
    as_bm_bitmap(pm, threshold = threshold)
}

#' @rdname bm_distort
#' @export
bm_distort.bm_list <- function(x, width = NULL, height = NULL, ...) {
    bm_lapply(x, bm_distort, width = width, height = height, ...)
}

#' @rdname bm_distort
#' @param filter Passed to [magick::image_resize()].
#'               Use [magick::filter_types()] for list of supported filters.
#'               The default "Point" filter will maintain your sprite's color palette.
#'               `NULL` will give you the `magick`'s default filter which may work better
#'               if you are not trying to maintain a sprite color palette.
#' @export
bm_distort.bm_pixmap <- function(x, width = NULL, height = NULL, ..., filter = "Point") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    stopifnot(!is.null(width) || !is.null(height))
    if (is.null(width) || is.null(height))
        geometry <- magick::geometry_size_pixels(width, height, TRUE)
    else
        geometry <- magick::geometry_size_pixels(width, height, FALSE)
    mi <- magick::image_resize(magick::image_read(x), geometry, filter = filter)
    as_bm_pixmap(mi)
}

#' @rdname bm_distort
#' @export
`bm_distort.magick-image` <- function(x, width = NULL, height = NULL, ..., filter = "Point") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    stopifnot(!is.null(width) || !is.null(height))
    if (is.null(width) || is.null(height))
        geometry <- magick::geometry_size_pixels(width, height, TRUE)
    else
        geometry <- magick::geometry_size_pixels(width, height, FALSE)
    magick::image_resize(x, geometry, filter = filter)
}

#' @rdname bm_distort
#' @export
bm_distort.nativeRaster <- function(x, width = NULL, height = NULL, ..., filter = "Point") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    stopifnot(!is.null(width) || !is.null(height))
    if (is.null(width) || is.null(height))
        geometry <- magick::geometry_size_pixels(width, height, TRUE)
    else
        geometry <- magick::geometry_size_pixels(width, height, FALSE)
    mi <- magick::image_resize(magick::image_read(x), geometry, filter = filter)
    as.raster(mi, native = TRUE)
}

#' @rdname bm_distort
#' @export
bm_distort.raster <- function(x, width = NULL, height = NULL, ..., filter = "Point") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    stopifnot(!is.null(width) || !is.null(height))
    if (is.null(width) || is.null(height))
        geometry <- magick::geometry_size_pixels(width, height, TRUE)
    else
        geometry <- magick::geometry_size_pixels(width, height, FALSE)
    mi <- magick::image_resize(magick::image_read(x), geometry, filter = filter)
    as.raster(mi)
}
