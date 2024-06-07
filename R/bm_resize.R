#' Resize bitmaps by trimming and/or extending
#'
#' Trim and/or extend bitmaps to a desired height and/or width.
#'
#' This function is a convenience wrapper around [bm_trim()] and [bm_extend()].
#'
#' @inheritParams bm_extend
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' # add a border to an "R"
#' capital_r <- font[[str2ucp("R")]]
#' print(capital_r)
#' capital_r <- bm_resize(capital_r, width = 12L, height = 12L, vjust = "top")
#' print(capital_r)
#' corn <- farming_crops_16x16()$corn$portrait
#' corn_rs <- bm_resize(corn, width = 20L, height = 20L, vjust = "top")
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_rs, bg = "cyan", compress = "v")
#' }
#' @seealso [bm_extend()], [bm_pad()], and [bm_trim()].
#' @inherit bm_clamp return
#' @export
bm_resize <- function(x, value, # nolint
                      width = NULL, height = NULL,
                      hjust = "center-left", vjust = "center-top") {
    UseMethod("bm_resize")
}

#' @rdname bm_resize
#' @export
bm_resize.bm_bitmap <- function(x, value = 0L, # nolint
                                width = NULL, height = NULL,
                                hjust = "center-left", vjust = "center-top") {
    bm_resize_bitmap(x, value = value,
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

#' @rdname bm_resize
#' @export
bm_resize.bm_list <- function(x, ...) {
    bm_lapply(x, bm_resize, ...)
}

#' @rdname bm_resize
#' @export
bm_resize.bm_pixmap <- function(x, value = col2hex("transparent"), # nolint
                                width = NULL, height = NULL,
                                hjust = "center-left", vjust = "center-top") {
    bm_resize_bitmap(x, value = value,
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

#' @rdname bm_resize
#' @export
`bm_resize.magick-image` <- function(x, value = "transparent", # nolint
                                     width = NULL, height = NULL,
                                     hjust = "center-left", vjust = "center-top") {
    bm_resize_bitmap(x, value = value,
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

#' @rdname bm_resize
#' @export
bm_resize.nativeRaster <- function(x, value = col2int("transparent"), # nolint
                             width = NULL, height = NULL,
                             hjust = "center-left", vjust = "center-top") {
    bm_resize_bitmap(x, value = as_native(value),
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

#' @rdname bm_resize
#' @export
bm_resize.raster <- function(x, value = "transparent", # nolint
                             width = NULL, height = NULL,
                             hjust = "center-left", vjust = "center-top") {
    bm_resize_bitmap(x, value = value,
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

bm_resize_bitmap <- function(x, value,
                             width = NULL, height = NULL,
                             hjust = hjust, vjust = vjust) {
    if (!is.null(width) && bm_widths(x) != width) {
        if (bm_widths(x) < width)
            x <- bm_extend(x, value = value, width = width, hjust = hjust)
        else
            x <- bm_trim(x, width = width, hjust = hjust)
    }
    if (!is.null(height) && bm_heights(x) != height) {
        if (bm_heights(x) < height)
            x <- bm_extend(x, value = value, height = height, vjust = vjust)
        else
            x <- bm_trim(x, height = height, vjust = vjust)
    }
    x
}
