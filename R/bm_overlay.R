#' Merge bitmaps by overlaying one over another
#'
#' `bm_overlay()` merges bitmaps by overlaying a bitmap over another.
#'
#' If necessary bitmaps will be extended by `bm_extend()` such that
#' they are the same size.  Then the non-zero pixels of the \dQuote{over}
#' bitmap will be inserted into the \dQuote{under} bitmap.
#'
#' @inheritParams bm_clamp
#' @inheritParams bm_extend
#' @param over A bitmap/pixmap object to overlay
#'             over the `x` bitmap(s).
#'             Only one of `over` or `under` may be set.
#' @param under A bitmap/pixmap object which will be overlaid
#'             by the `x` bitmap(s).
#'             Only one of `over` or `under` may be set.
#' @inherit bm_clamp return
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' grave <- font[[str2ucp("`")]]
#' a <- font[[str2ucp("a")]]
#' a_grave <- bm_overlay(a, over = grave)
#' print(a_grave)
#'
#' # Can also instead specify the under glyph as a named argument
#' a_grave2 <- bm_overlay(grave, under = a)
#' print(a_grave2)
#'
#' crops <- farming_crops_16x16()
#' corn <- bm_shift(crops$corn$portrait, right = 2L, top = 2L)
#' grapes <- bm_shift(crops$grapes$portrait, bottom = 1L)
#' grapes_and_corn <- bm_overlay(corn, grapes)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(grapes_and_corn, compress = "v")
#' }
#' @export
bm_overlay <- function(x, over = NULL, under = NULL,
                       hjust = "center-left", vjust = "center-top", ...) {
    stopifnot(is.null(over) || is.null(under))
    UseMethod("bm_overlay")
}

#' @rdname bm_overlay
#' @param bg Bitmap background value.
#' @export
bm_overlay.bm_bitmap <- function(x, over = NULL, under = NULL,
                                 hjust = "center-left", vjust = "center-top", bg = 0L, ...) {
    if (is.null(over)) {
        over <- x
        under <- as_bm_bitmap(under)
    } else {
        under <- x
        over <- as_bm_bitmap(over)
    }
    bm_overlay_bitmap(over = over, under = under,
                      hjust = hjust, vjust = vjust, bg = bg)
}

#' @rdname bm_overlay
#' @export
bm_overlay.bm_list <- function(x, ...) {
    bm_lapply(x, bm_overlay, ...)
}

#' @rdname bm_overlay
#' @export
bm_overlay.bm_pixmap <- function(x, over = NULL, under = NULL,
                                 hjust = "center-left", vjust = "center-top",
                                 bg = col2hex("transparent"), ...) {
    bg <- col2hex(bg)
    if (is.null(over)) {
        over <- x
        under <- as_bm_pixmap(under)
    } else {
        under <- x
        over <- as_bm_pixmap(over)
    }
    bm_overlay_bitmap(over = over, under = under,
                      hjust = hjust, vjust = vjust, bg = bg)
}

#' @rdname bm_overlay
#' @export
`bm_overlay.magick-image` <- function(x, over = NULL, under = NULL,
                                      hjust = "center-left", vjust = "center-top",
                                      bg = "transparent", ...) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    x <- as_bm_pixmap(x)
    bg <- col2hex(bg)
    if (is.null(over)) {
        over <- x
        under <- as_bm_pixmap(under)
    } else {
        under <- x
        over <- as_bm_pixmap(over)
    }
    pm <- bm_overlay_bitmap(over = over, under = under,
                            hjust = hjust, vjust = vjust, bg = bg)
    magick::image_read(pm)
}

#' @rdname bm_overlay
#' @export
bm_overlay.nativeRaster <- function(x, over = NULL, under = NULL,
                              hjust = "center-left", vjust = "center-top",
                              bg = col2int("transparent"), ...) {
    x <- as_bm_pixmap(x)
    bg <- int2col(as_native(bg))
    if (is.null(over)) {
        over <- x
        under <- as_bm_pixmap(under)
    } else {
        under <- x
        over <- as_bm_pixmap(over)
    }
    pm <- bm_overlay_bitmap(over = over, under = under,
                            hjust = hjust, vjust = vjust, bg = bg)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_overlay
#' @export
bm_overlay.raster <- function(x, over = NULL, under = NULL,
                              hjust = "center-left", vjust = "center-top",
                              bg = "transparent", ...) {
    x <- as_bm_pixmap(x)
    bg <- col2hex(bg)
    if (is.null(over)) {
        over <- x
        under <- as_bm_pixmap(under)
    } else {
        under <- x
        over <- as_bm_pixmap(over)
    }
    pm <- bm_overlay_bitmap(over = over, under = under,
                            hjust = hjust, vjust = vjust, bg = bg)
    as.raster(pm)
}

bm_overlay_bitmap <- function(over = NULL, under = NULL,
                              hjust = "center-left", vjust = "center-top", bg) {
    if (bm_widths(over) > bm_widths(under))
        under <- bm_extend(under, width = bm_widths(over), hjust = hjust)
    if (bm_widths(under) > bm_widths(over))
        over <- bm_extend(over, width = bm_widths(under), hjust = hjust)
    if (bm_heights(over) > bm_heights(under))
        under <- bm_extend(under, height = bm_heights(over), vjust = vjust)
    if (bm_heights(under) > bm_heights(over))
        over <- bm_extend(over, height = bm_heights(under), vjust = vjust)
    indices <- which(as.logical(over != bg))
    under[indices] <- over[indices]
    under
}
