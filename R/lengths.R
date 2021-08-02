#' Widths or heights of bitmaps
#'
#' `bm_widths()` returns the widths of the bitmaps while
#' `bm_heights()` returns the heights of the bitmaps.
#'
#' @inheritParams bm_clamp
#' @param unique Apply [base::unique()] to the returned integer vector.
#' @return A integer vector of the relevant length of each
#'         of the `bm_bitmap()` objects in `x`.
#'         If `unique` is `TRUE` then any duplicates will have been removed.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   bm_widths(font) # every glyph in the font is 8 pixels wide
#'   bm_heights(font) # every glyph in the font is 16 pixels high
#' @rdname lengths
#' @export
bm_widths <- function(bm_object, unique = TRUE) {
    stopifnot(is_bm_object(bm_object))
    if (is_bm_bitmap(bm_object))
        nc <- ncol(bm_object)
    else
        nc <- sapply(bm_object, ncol)
    if (unique)
        base::unique(nc)
    else
        nc
}

#' @rdname lengths
#' @export
bm_heights <- function(bm_object, unique = TRUE) {
    stopifnot(is_bm_object(bm_object))
    if (is_bm_bitmap(bm_object))
        nr <- nrow(bm_object)
    else
        nr <- sapply(bm_object, nrow)
    if (unique)
        base::unique(nr)
    else
        nr
}
