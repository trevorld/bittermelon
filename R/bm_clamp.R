#' Clamp bitmap glyph integer values.
#'
#' `bm_clamp()` \dQuote{clamps} bitmap glyph integers to lie between a lower and upper value.
#' The default lower and upper values coerce the bitmap glyph
#' into a binary bitmap glyph (as expected by most bitmap font formats).
#'
#' @param bm_object Either a [bm_glyph()] or [bm_list()] object (including [bm_font()]).
#' @param lower Integer value.  Any value below `lower` will be clamped to `lower`.
#' @param upper Integer value.  Any value above `upper` will be clamped to `upper`.
#' @export
bm_clamp <- function(bm_object, lower = 0L, upper = 1L) {
    modify_bm_glyphs(bm_object, bm_clamp_glyph, lower = lower, upper = upper)
}

bm_clamp_glyph <- function(glyph, lower = 0L, upper = 1L) { # nolint
    stopifnot(lower <= upper)
    indices <- which(glyph < lower)
    if (length(indices)) {
        glyph[indices] <- lower
    }
    indices <- which(glyph > upper)
    if (length(indices)) {
        glyph[indices] <- upper
    }
    glyph
}
