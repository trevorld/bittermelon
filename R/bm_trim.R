#' Trim bitmaps
#'
#' `bm_trim()` trims bitmap objects reducing the number of pixels.
#' The directions and amount of removed pixels are settable.
#'
#' @inheritParams bm_clamp
#' @param sides If not `NULL` then an integer vector indicating how
#'              many pixels to trim on all four sides.
#'              If the integer vector is of length one it indicates the number of pixels for all four sides.
#'              If of length two gives first the number for the vertical sides and then the horizontal sides.
#'              If of length three gives the number of pixels for top, the horizontal sides, and then bottom sides.
#'              If of length four gives the number of pixels for top, right, bottom, and then left sides.
#'              This is the same scheme as used by the CSS padding and margin properties.
#' @param top How many pixels to trim the top.
#' @param right How many pixels to trim the right.
#' @param bottom How many pixels to trim the bottom.
#' @param left How many pixels to trim the left.
#' @param width How many pixels wide should the new bitmap be.
#'              Use with the `hjust` argument or just one of either the `left` or `right` arguments.
#' @param height How many pixels tall should the new bitmap be.
#'              Use with the `vjust` argument or just one of either the `top` or `bottom` arguments.
#' @param hjust One of "left", "center-left", "center-right", "right".
#'              "center-left" and "center-right" will attempt to
#'              place in "center" if possible but if not possible will bias
#'              it one pixel left or right respectively.
#'              "centre", "center", and "centre-left" are aliases for "center-left".
#'              "centre-right" is an alias for "center-right".
#'              Note if "left" we will trim on the right (and vice-versa).
#' @param vjust One of "bottom", "center-bottom", "center-top", "top".
#'              "center-bottom" and "center-top" will attempt to
#'              place in "center" if possible but if not possible will bias
#'              it one pixel down or up respectively.
#'              "centre", "center", and "centre-top" are aliases for "center-top".
#'              "centre-bottom" is an alias for "center-bottom".
#'              Note if "top" we will trim on the bottom (and vice-versa).
#' @inherit bm_clamp return
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(capital_r)
#' capital_r_trimmed <- bm_trim(capital_r, c(1, 1, 3, 0))
#' print(capital_r_trimmed)
#' corn <- farming_crops_16x16()$corn$portrait
#' print(bm_padding_lengths(corn))
#' corn_trimmed <- bm_trim(corn, top = 1L, right = 1L, bottom = 1L)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_trimmed, bg = "cyan", compress = "v")
#' }
#' @seealso [bm_extend()], [bm_pad()], and [bm_resize()].
#' @export
bm_trim <- function(x, sides = NULL,
                    top = NULL, right = NULL, bottom = NULL, left = NULL,
                    width = NULL, height = NULL,
                    hjust = "center-left", vjust = "center-top") {
    stopifnot(missing(sides) || missing(top))
    stopifnot(missing(sides) || missing(right))
    stopifnot(missing(sides) || missing(bottom))
    stopifnot(missing(sides) || missing(left))
    stopifnot(missing(height) || (missing(top)) || missing(bottom))
    stopifnot(missing(width) || (missing(left)) || missing(right))
    stopifnot(missing(hjust) || (missing(left) && missing(right)))
    stopifnot(missing(vjust) || (missing(left) && missing(right)))
    UseMethod("bm_trim")
}

#' @rdname bm_trim
#' @export
bm_trim.bm_matrix <- function(x, sides = NULL,
                              top = NULL, right = NULL, bottom = NULL, left = NULL,
                              width = NULL, height = NULL,
                              hjust = "center-left", vjust = "center-top") {
    bm_trim_bitmap(x,
                   sides = sides,
                   top = top, right = right, bottom = bottom, left = left,
                   width = width, height = height,
                   hjust = hjust, vjust = vjust)
}

#' @rdname bm_trim
#' @export
bm_trim.bm_list <- function(x, ...) {
    bm_lapply(x, bm_trim, ...)
}

#' @rdname bm_trim
#' @export
`bm_trim.magick-image` <- function(x, sides = NULL,
                                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                                   width = NULL, height = NULL,
                                   hjust = "center-left", vjust = "center-top") {
    bm_trim_bitmap(x,
                   sides = sides,
                   top = top, right = right, bottom = bottom, left = left,
                   width = width, height = height,
                   hjust = hjust, vjust = vjust)
}

#' @rdname bm_trim
#' @export
bm_trim.nativeRaster <- function(x, sides = NULL,
                                 top = NULL, right = NULL, bottom = NULL, left = NULL,
                                 width = NULL, height = NULL,
                                 hjust = "center-left", vjust = "center-top") {
    # Use `nara::nr_crop()` or `nara::nr_crop2()`?
    pm <- as_bm_pixmap.nativeRaster(x)
    pm <- bm_trim_bitmap(pm,
                         sides = sides,
                         top = top, right = right, bottom = bottom, left = left,
                         width = width, height = height,
                         hjust = hjust, vjust = vjust)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_trim
#' @export
bm_trim.raster <- function(x, sides = NULL,
                           top = NULL, right = NULL, bottom = NULL, left = NULL,
                           width = NULL, height = NULL,
                           hjust = "center-left", vjust = "center-top") {
    bm_trim_bitmap(x,
                   sides = sides,
                   top = top, right = right, bottom = bottom, left = left,
                   width = width, height = height,
                   hjust = hjust, vjust = vjust)
}

bm_trim_bitmap <- function(x, sides = NULL,
                           top = NULL, right = NULL, bottom = NULL, left = NULL,
                           width = NULL, height = NULL,
                           hjust = "center-left", vjust = "center-top") {
    d <- list(top = top %||% 0L, right = right %||% 0L,
              bottom = bottom %||% 0L, left = left %||% 0L)

    nc <- bm_widths(x)
    nr <- bm_heights(x)

    if (!is.null(sides))
        d <- adjust_d_sides(sides, d)
    if (!is.null(width))
        d <- adjust_d_width_trim(x, width, hjust, d, left, right)
    if (!is.null(height))
        d <- adjust_d_height_trim(x, height, vjust, d, top, bottom)
    stopifnot(min(unlist(d)) >= 0L)
    stopifnot(nc >= d$left + d$right)
    stopifnot(nr >= d$top + d$bottom)

    bm_trim_simple(x, d$top, d$right, d$bottom, d$left)
}

bm_trim_simple <- function(x, top = 0L, right = 0L, bottom = 0L, left = 0L) {
    UseMethod("bm_trim_simple")
}

#' @export
bm_trim_simple.bm_matrix <- function(x, top = 0L, right = 0L, bottom = 0L, left = 0L) {
    x[seq.int(1L + bottom, nrow(x) - top),
      seq.int(1L + left, ncol(x) - right),
      drop = FALSE]
}

#' @export
`bm_trim_simple.magick-image` <- function(x, top = 0L, right = 0L, bottom = 0L, left = 0L) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    geometry <- sprintf("%dx%d+%d+%d",
                        magick::image_info(x)$width - left - right,
                        magick::image_info(x)$height - top - bottom,
                        left, top)
    magick::image_crop(x, geometry)
}

#' @export
bm_trim_simple.raster <- function(x, top = 0L, right = 0L, bottom = 0L, left = 0L) {
    x[seq.int(1L + top, nrow(x) - bottom),
      seq.int(1L + left, ncol(x) - right),
      drop = FALSE]
}

adjust_d_width_trim <- function(bitmap, width, hjust, d, left, right) {
    stopifnot(width <= ncol(bitmap))
    remainder <- ncol(bitmap) - width
    adjust_d_width(remainder, width, hjust, d, left, right)
}

adjust_d_height_trim <- function(bitmap, height, vjust, d, top, bottom) {
    stopifnot(height <= nrow(bitmap))
    remainder <- nrow(bitmap) - height
    adjust_d_height(remainder, height, vjust, d, top, bottom)
}
