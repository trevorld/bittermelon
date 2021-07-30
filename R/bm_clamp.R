#' Clamp bitmap integer values.
#'
#' `bm_clamp()` \dQuote{clamps} bitmap integers to lie between a lower and upper value.
#' The default lower and upper values coerce the bitmap
#' into a binary bitmap (as expected by most bitmap font formats).
#'
#' @param bm_object Either a [bm_bitmap()] or [bm_list()] object (including [bm_font()]).
#' @param lower Integer value.  Any value below `lower` will be clamped to `lower`.
#' @param upper Integer value.  Any value above `upper` will be clamped to `upper`.
#' @export
bm_clamp <- function(bm_object, lower = 0L, upper = 1L) {
    modify_bm_bitmaps(bm_object, bm_clamp_bitmap, lower = lower, upper = upper)
}

bm_clamp_bitmap <- function(bitmap, lower = 0L, upper = 1L) { # nolint
    stopifnot(lower <= upper)
    indices <- which(bitmap < lower)
    if (length(indices)) {
        bitmap[indices] <- lower
    }
    indices <- which(bitmap > upper)
    if (length(indices)) {
        bitmap[indices] <- upper
    }
    bitmap
}
