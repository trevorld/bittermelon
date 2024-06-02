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
#'  print(capital_r)
#'  capital_r <- bm_shift(capital_r, bottom = 2L, right = 1L)
#'  print(capital_r)
#' @seealso [bm_trim()] and [bm_extend()]
#' @export
bm_shift <- function(x, value = 0L,
                     top = NULL, right = NULL, bottom = NULL, left = NULL) {
    stopifnot(is.null(top) || is.null(bottom))
    stopifnot(is.null(left) || is.null(right))
    modify_bm_bitmaps(x, bm_shift_bitmap, value = value,
                     top = top, right = right, bottom = bottom, left = left)
}

bm_shift_bitmap <- function(x, value = 0L,
                     top = NULL, right = NULL, bottom = NULL, left = NULL) {
    x <- bm_trim(x, top = top, right = right, bottom = bottom, left = left)
    x <- bm_extend(x, value = value, top = bottom, right = left, bottom = top, left = right)
    x
}
