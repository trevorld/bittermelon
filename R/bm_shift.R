#' Shift elements within bitmaps
#'
#' Shifts non-padding elements within bitmaps by trimming on a specified side and padding on the other
#' while preserving the width and height of the original bitmap.
#'
#' This function is a convenience wrapper around [bm_trim()] and [bm_extend()].
#'
#' @inheritParams bm_extend
#' @param top Number of pixels to shift towards the top side.
#' @param right Number of pixels to shift towards the right side.
#' @param bottom Number of pixels to shift towards the bottom side.
#' @param left Number of pixels to shift towards the left side.
#' @inherit bm_clamp return
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = c("-", "#"))
#'  capital_r <- bm_shift(capital_r, bottom = 2L, right = 1L)
#'  print(capital_r, px = c("-", "#"))
#' @seealso [bm_trim()] and [bm_extend()]
#' @export
bm_shift <- function(bm_object, value = 0L,
                     top = NULL, right = NULL, bottom = NULL, left = NULL) {
    stopifnot(is.null(top) || is.null(bottom))
    stopifnot(is.null(left) || is.null(right))
    modify_bm_bitmaps(bm_object, bm_shift_bitmap, value = value,
                     top = top, right = right, bottom = bottom, left = left)
}

bm_shift_bitmap <- function(bitmap, value = 0L,
                     top = NULL, right = NULL, bottom = NULL, left = NULL) {
    bitmap <- bm_trim(bitmap, top = top, right = right, bottom = bottom, left = left)
    bitmap <- bm_extend(bitmap, value = value, top = bottom, right = left, bottom = top, left = right)
    bitmap
}
