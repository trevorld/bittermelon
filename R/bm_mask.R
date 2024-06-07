#' Modify bitmaps via masking with a 'mask' bitmap
#'
#' `bm_mask()` modifies bitmaps by using a binary bitmap \dQuote{mask}
#' to set certain elements to a background value.
#'
#' If necessary bitmaps will be extended by `bm_extend()` such that
#' they are the same size.
#' If necessary the `mask` will be coerced into a \dQuote{binary} mask
#' by `bm_clamp(as_bm_bitmap(mask))`.
#' If `mode` is "luminance" then where the `mask` is `1L`
#' the corresponding pixel in `base` will be coerced to the background value.
#' If `mode` is "alpha" then where the `mask` is `0L`
#' the corresponding pixel in `base` will be coerced to the background value.
#'
#' @inheritParams bm_clamp
#' @inheritParams bm_overlay
#' @param mask An object to use as a binary bitmap \dQuote{mask}.
#'             Only one of `mask` or `base` may be set.
#'             Will be coerced to a [bm_bitmap()] object by [as_bm_bitmap()].
#' @param base A bitmap/pixmap object which will be \dQuote{masked} by `mask`.
#'             Only one of `mask` or `base` may be set.
#' @param mode Either "luminance" (default) or "alpha".
#' @return A bitmap/pixmap object that is the same type as `x` (if `base` is `NULL`) or `base`.
#' @examples
#' if (require("grid", quietly = TRUE) && capabilities("png")) {
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   one <- font[[str2ucp("1")]]
#'   circle_large <- as_bm_bitmap(circleGrob(r = 0.50), width = 16L, height = 16L)
#'   circle_small <- as_bm_bitmap(circleGrob(r = 0.40), width = 16L, height = 16L)
#'   circle_outline <- bm_mask(circle_large, circle_small)
#'   print(circle_outline)
#' }
#' if (capabilities("png")) {
#'   # U+2776 "Dingbat Negative Circled Digit One"
#'   circle_minus_one <- bm_mask(circle_large, one)
#'   print(circle_minus_one)
#' }
#' # Can also do "alpha" mask
#' square_full <- bm_bitmap(matrix(1L, nrow = 16L, ncol = 16L))
#' square_minus_lower_left <- square_full
#' square_minus_lower_left[1:8, 1:8] <- 0L
#' print(square_minus_lower_left)
#' if (capabilities("png")) {
#'   circle_minus_lower_left <- bm_mask(circle_large, square_minus_lower_left, mode = "alpha")
#'   print(circle_minus_lower_left)
#' }
#' 
#' if (capabilities("png")) {
#'   m <- matrix(grDevices::rainbow(8L), byrow = TRUE, ncol = 8L, nrow = 8L)
#'   rainbow <- bm_expand(as_bm_pixmap(m), 2L)
#'   circle_rainbow <- bm_mask(rainbow, circle_large, mode = "alpha")
#' }
#' if (cli::is_utf8_output() && 
#'     cli::num_ansi_colors() >= 256L &&
#'     capabilities("png")) {
#'   print(circle_rainbow, compress = "v")
#' }
#' @export
bm_mask <- function(x, mask = NULL, base = NULL,
                    mode = c("luminance", "alpha"),
                    hjust = "center-left", vjust = "center-top") {
    stopifnot(is.null(mask) || is.null(base))
    if (!is.null(base))
        bm_mask(base, mask = x, mode = mode, hjust = hjust, vjust = vjust)
    else
        UseMethod("bm_mask")
}

#' @rdname bm_mask
#' @export
bm_mask.bm_bitmap <- function(x, mask = NULL, base = NULL,
                              mode = c("luminance", "alpha"),
                              hjust = "center-left", vjust = "center-top") {
    mode <- match.arg(mode, c("luminance", "alpha"))
    if (is.null(mask)) {
        mask <- x
    } else {
        base <- x
    }
    base <- as_bm_bitmap(base)
    bm_mask_bitmap(mask = mask, base = base, mode = mode,
                   hjust = hjust, vjust = vjust, bg = 0L)
}

#' @rdname bm_mask
#' @export
bm_mask.bm_pixmap <- function(x, mask = NULL, base = NULL,
                              mode = c("luminance", "alpha"),
                              hjust = "center-left", vjust = "center-top") {
    mode <- match.arg(mode, c("luminance", "alpha"))
    if (is.null(mask)) {
        mask <- x
    } else {
        base <- x
    }
    base <- as_bm_pixmap(base)
    bm_mask_bitmap(mask = mask, base = base, mode = mode,
                   hjust = hjust, vjust = vjust, bg = "#FFFFFF00")
}

#' @rdname bm_mask
#' @export
`bm_mask.magick-image` <- function(x, mask = NULL, base = NULL,
                                   mode = c("luminance", "alpha"),
                                   hjust = "center-left", vjust = "center-top") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    mode <- match.arg(mode, c("luminance", "alpha"))
    if (is.null(mask)) {
        mask <- x
    } else {
        base <- x
    }
    base <- as_bm_pixmap(base)
    pm <- bm_mask_bitmap(mask = mask, base = base, mode = mode,
                         hjust = hjust, vjust = vjust, bg = "#FFFFFF00")
    magick::image_read(pm)
}

#' @rdname bm_mask
#' @export
bm_mask.nativeRaster <- function(x, mask = NULL, base = NULL,
                                 mode = c("luminance", "alpha"),
                                 hjust = "center-left", vjust = "center-top") {
    mode <- match.arg(mode, c("luminance", "alpha"))
    if (is.null(mask)) {
        mask <- x
    } else {
        base <- x
    }
    base <- as_bm_pixmap(base)
    pm <- bm_mask_bitmap(mask = mask, base = base, mode = mode,
                         hjust = hjust, vjust = vjust, bg = "#FFFFFF00")
    as.raster(pm, native = TRUE)
}

#' @rdname bm_mask
#' @export
bm_mask.raster <- function(x, mask = NULL, base = NULL,
                              mode = c("luminance", "alpha"),
                              hjust = "center-left", vjust = "center-top") {
    mode <- match.arg(mode, c("luminance", "alpha"))
    if (is.null(mask)) {
        mask <- x
    } else {
        base <- x
    }
    base <- as_bm_pixmap(base)
    pm <- bm_mask_bitmap(mask = mask, base = base, mode = mode,
                         hjust = hjust, vjust = vjust, bg = "#FFFFFF00")
    as.raster(pm)
}

#' @rdname bm_mask
#' @export
bm_mask.bm_list <- function(x, mask = NULL, base = NULL,
                            mode = c("luminance", "alpha"),
                            hjust = "center-left", vjust = "center-top") {
    bm_lapply(x, bm_mask,
              mask = mask, base = base, mode = mode,
              hjust = hjust, vjust = vjust)
}

bm_mask_bitmap <- function(x, mask = NULL, base = NULL, mode = "luminance",
                           hjust = "center-left", vjust = "center-top", bg = 0L) {
    mask <- bm_clamp(as_bm_bitmap(mask)) # coerce to binary bitmap
    if (ncol(mask) > ncol(base))
        base <- bm_extend(base, value = bg, width = ncol(mask), hjust = hjust)
    if (ncol(base) > ncol(mask))
        mask <- bm_extend(mask, width = ncol(base), hjust = hjust)
    if (nrow(mask) > nrow(base))
        base <- bm_extend(base, value = bg, height = nrow(mask), vjust = vjust)
    if (nrow(base) > nrow(mask))
        mask <- bm_extend(mask, height = nrow(base), vjust = vjust)


    if (mode == "luminance") # 'luminance' mode then > 0 hidden
        indices <- which(as.logical(mask == 1L))
    else # 'alpha' mode then < 1 hidden
        indices <- which(as.logical(mask == 0L))
    base[indices] <- bg
    base
}
