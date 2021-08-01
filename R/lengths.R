#' Widths or heights of bitmaps in bitmap lists
#'
#' `bm_widths()` returns the widths of the bitmaps in a `bm_list()` while
#' `bm_heights()` returns the heights of the bitmaps.
#'
#' @param x A `bm_list()` object including a `bm_font()` object.
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
bm_widths <- function(x, unique = TRUE) {
    nc <- sapply(x, ncol)
    if (unique)
        base::unique(nc)
    else
        nc
}

#' @rdname lengths
#' @export
bm_heights <- function(x, unique = TRUE) {
    nr <- sapply(x, nrow)
    if (unique)
        base::unique(nr)
    else
        nr
}
