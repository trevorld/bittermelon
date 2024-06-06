#' Widths or heights of bitmaps
#'
#' `bm_widths()` returns the widths of the bitmaps while
#' `bm_heights()` returns the heights of the bitmaps.
#' `bm_widths()` and `bm_heights()` are S3 generic functions.
#'
#' @inheritParams bm_clamp
#' @param unique Apply [base::unique()] to the returned integer vector.
#' @param ... Ignored.
#' @return A integer vector of the relevant length of each
#'         of the bitmap objects in `x`.
#'         If `unique` is `TRUE` then any duplicates will have been removed.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' bm_widths(font) # every glyph in the font is 8 pixels wide
#' bm_heights(font) # every glyph in the font is 16 pixels high
#' corn <- farming_crops_16x16()$corn$portrait
#' bm_widths(corn)
#' bm_heights(corn)
#' @rdname lengths
#' @export
bm_heights <- function(x, ...) {
    UseMethod("bm_heights")
}

#' @rdname lengths
#' @export
bm_heights.bm_matrix <- function(x, ...) {
    nrow(x)
}

#' @rdname lengths
#' @export
bm_heights.bm_list <- function(x, unique = TRUE, ...) {
    nr <- vapply(x, nrow, integer(1L))
    if (unique)
        base::unique(nr)
    else
        nr
}

#' @rdname lengths
#' @export
`bm_heights.magick-image` <- function(x, ...) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    magick::image_info(x)$height
}

#' @rdname lengths
#' @export
bm_heights.nativeRaster <- function(x, ...) {
    nrow(x)
}

#' @rdname lengths
#' @export
bm_heights.raster <- function(x, ...) {
    nrow(x)
}

#' @rdname lengths
#' @export
bm_widths <- function(x, ...) {
    UseMethod("bm_widths")
}

#' @rdname lengths
#' @export
bm_widths.bm_matrix <- function(x, ...) {
    ncol(x)
}

#' @rdname lengths
#' @export
bm_widths.bm_list <- function(x, unique = TRUE, ...) {
    nc <- vapply(x, ncol, integer(1L))
    if (unique)
        base::unique(nc)
    else
        nc
}

#' @rdname lengths
#' @export
`bm_widths.magick-image` <- function(x, ...) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    magick::image_info(x)$width
}

#' @rdname lengths
#' @export
bm_widths.nativeRaster <- function(x, ...) {
    ncol(x)
}

#' @rdname lengths
#' @export
bm_widths.raster <- function(x, ...) {
    ncol(x)
}
