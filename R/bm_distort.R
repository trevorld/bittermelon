#' Resize bitmaps via distortion.
#'
#' `bm_distort()` resize bitmaps to arbitrary width and height value via [magick::image_resize()].
#' `bm_downscale()` is a wrapper to `bm_distort()` that downscales an image if (and only if) it is
#' wider than a target width.
#'
#' @inheritParams bm_clamp
#' @inheritParams as_bm_bitmap.grob
#' @inherit bm_clamp return
#'
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' dim(capital_r) # 8 x 16
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   capital_r_9x21 <- bm_distort(capital_r, width = 9L, height = 21L)
#'   print(capital_r_9x21)
#' }
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' dim(corn) # 16 x 16
#' if (cli::is_utf8_output() && 
#'     cli::num_ansi_colors() >= 256L &&
#'     requireNamespace("magick", quietly = TRUE)) {
#'   corn_24x24 <- bm_distort(corn, width = 24L)
#'   print(corn_24x24, compress = "v")
#' }
#' @seealso [bm_expand()] for expanding width/height by integer multiples.
#'          [bm_resize()] for resizing an image via trimming/extending an image.
#' @export
bm_distort <- function(x, width = NULL, height = NULL, ...) {
    UseMethod("bm_distort")
}

#' @rdname bm_distort
#' @export
bm_downscale <- function(x, width = getOption("width"), ...) {
    if (bm_widths(x) > width)
        bm_distort(x, width = width, ...)
    else
        x
}

downscale_for_terminal <- function(x, direction, filter = "Point") {
    if (direction %in% c("h", "b")) {
        multiplier <- 2L
    } else {
        multiplier <- 1L
    }
    bm_downscale(x, width = multiplier * getOption("width"), filter = filter)
}

#' @rdname bm_distort
#' @param threshold  When the alpha channel
#'                   weakly exceeds this threshold
#'                   (on an interval from zero to one)
#'                   then the pixel is determined to be \dQuote{black}.
#' @export
bm_distort.bm_bitmap <- function(x, width = NULL, height = NULL, ...,
                                 filter = "Point", threshold = 0.50) {
    pm <- bm_distort(as_bm_pixmap(x), width, height, filter = filter)
    as_bm_bitmap(pm, threshold = threshold)
}

#' @rdname bm_distort
#' @export
bm_distort.bm_list <- function(x, ...) {
    bm_lapply(x, bm_distort, ...)
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
