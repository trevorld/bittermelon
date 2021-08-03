#' Clamp bitmap integer values.
#'
#' `bm_clamp()` \dQuote{clamps} bitmap integers that lie outside an interval.
#' The default coerces a multiple-integer-valued bitmap
#' into a binary bitmap (as expected by most bitmap font formats).
#'
#' @param bm_object Either a [bm_bitmap()], [bm_list()],  or [bm_font()] object.
#' @param lower Integer value.  Any value below `lower` will be clamped.
#' @param upper Integer value.  Any value above `upper` will be clamped.
#' @param value Integer vector of length one or two of replacement value(s).
#'              If `value` is length one
#'              any values above `upper` are replaced by `value`
#'              while those below `lower` are replaced by `lower`.
#'              If `value` is length two any values above `upper`
#'              are replaced by `value[2]` and any values below `lower`
#'              are replaced by `value[1]`.
#' @examples
#'  plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'  plus_sign[5L, 3:7] <- 2L
#'  plus_sign[3:7, 5L] <- 2L
#'  plus_sign_glyph <- bm_bitmap(plus_sign)
#'  print(plus_sign_glyph, px = c(".", "#", "@"))
#'
#'  plus_sign_clamped <- bm_clamp(plus_sign_glyph)
#'  print(plus_sign_clamped, px = c(".", "#", "@"))
#' @return Either a [bm_bitmap()], [bm_list()], or [bm_font()] object.
#' @export
bm_clamp <- function(bm_object, lower = 0L, upper = 1L, value = upper) {
    stopifnot(length(value) > 0L, length(value) < 3L)
    if (length(value) == 2L) {
        value_lower <- value[2L]
        value_upper <- value[2L]
    } else {
        value_lower <- lower
        value_upper <- value
    }
    modify_bm_bitmaps(bm_object, bm_clamp_bitmap, lower = lower, upper = upper,
                     value_lower = value_lower, value_upper = value_upper)
}

bm_clamp_bitmap <- function(bitmap, lower = 0L, upper = 1L,
                            value_lower = lower, value_upper = upper) {
    stopifnot(lower <= upper)
    indices <- which(bitmap < lower)
    if (length(indices)) {
        bitmap[indices] <- value_lower
    }
    indices <- which(bitmap > upper)
    if (length(indices)) {
        bitmap[indices] <- value_upper
    }
    bitmap
}
