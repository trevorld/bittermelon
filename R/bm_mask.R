#' Modify bitmaps via masking with a 'mask' bitmap
#'
#' `bm_mask()` modifies bitmaps by using a binary bitmap \dQuote{mask}
#' to set certain elements to zero.
#'
#' If necessary bitmaps will be extended by `bm_extend()` such that
#' they are the same size.
#' If necessary the `mask` will be coerced into a \dQuote{binary} mask.
#' If `mode` is "luminance" then where the `mask` is `1L`
#' the corresponding pixel in `base` will be coerced to `0L`.
#' If `mode` is "alpha" then where the `mask` is `0L`
#' the corresponding pixel in `base` will be coerced to `0L`
#'
#' @inheritParams bm_overlay
#' @param mask A 'bm_bitmap()' object to use as a \dQuote{mask}.
#'             Only one of `mask` or `base` may be set.
#' @param base A 'bm_bitmap()' object which will be \dQuote{masked} by `mask`.
#'             Only one of `mask` or `base` may be set.
#' @param mode Either "luminance" (default) or "alpha".
#' @inherit bm_clamp return
#' @examples
#'  if (require("grid") && capabilities("png")) {
#'    font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'    font <- read_hex(font_file)
#'    one <- font[[str2ucp("1")]]
#'    circle_large <- as_bm_bitmap(circleGrob(r = 0.50), width = 16L, height = 16L)
#'    circle_small <- as_bm_bitmap(circleGrob(r = 0.40), width = 16L, height = 16L)
#'
#'    circle_outline <- bm_mask(circle_large, circle_small)
#'    print(circle_outline, px = px_ascii)
#'
#'    # U+2776 "Dingbat Negative Circled Digit One"
#'    circle_minus_one <- bm_mask(circle_large, one)
#'    print(circle_minus_one, px = px_ascii)
#'
#'    # Can also do "alpha" mask
#'    square_full <- bm_bitmap(matrix(1L, nrow = 16L, ncol = 16L))
#'    square_minus_lower_left <- square_full
#'    square_minus_lower_left[1:8, 1:8] <- 0L
#'    print(square_minus_lower_left, px = px_ascii)
#'
#'    circle_minus_lower_left <- bm_mask(circle_large, square_minus_lower_left, mode = "alpha")
#'    print(circle_minus_lower_left, px = px_ascii)
#'  }
#' @export
bm_mask <- function(bm_object, mask = NULL, base = NULL,
                    mode = c("luminance", "alpha"),
                    hjust = "center-left", vjust = "center-top") {

    stopifnot(is.null(mask) || is.null(base))
    mode <- match.arg(mode, c("luminance", "alpha"))

    modify_bm_bitmaps(bm_object, bm_mask_bitmap,
                      mask = mask, base = base, mode = mode,
                      hjust = hjust, vjust = vjust)
}

bm_mask_bitmap <- function(bitmap, mask = NULL, base = NULL, mode = "luminance",
                           hjust = "center-left", vjust = "center-top") {
    if (is.null(mask)) {
        mask <- bitmap
        stopifnot(is_bm_bitmap(base))
    } else {
        base <- bitmap
        stopifnot(is_bm_bitmap(mask))
    }
    mask <- bm_clamp(mask) # coerce to binary
    if (ncol(mask) > ncol(base))
        base <- bm_extend(base, width = ncol(mask), hjust = hjust)
    if (ncol(base) > ncol(mask))
        mask <- bm_extend(mask, width = ncol(base), hjust = hjust)
    if (nrow(mask) > nrow(base))
        base <- bm_extend(base, height = nrow(mask), vjust = vjust)
    if (nrow(base) > nrow(mask))
        mask <- bm_extend(mask, height = nrow(base), vjust = vjust)


    if (mode == "luminance") # 'luminance' mode then > 0 hidden
        indices <- which(mask == 1L)
    else # 'alpha' mode then < 1 hidden
        indices <- which(mask == 0L)
    base[indices] <- 0L
    base
}
