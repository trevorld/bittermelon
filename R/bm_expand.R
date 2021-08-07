#' Expand bitmaps by repeating each row and/or column
#'
#' `bm_expand()` expands bitmap(s) by repeating each row and/or column
#' an indicated number of times.
#'
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @param width An integer of how many times to repeat each column.
#' @param height An integer of how many times to repeat each row.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = px_ascii)
#'  print(bm_expand(capital_r, width = 2L),
#'        px = px_ascii)
#'  print(bm_expand(capital_r, height = 2L),
#'        px = px_ascii)
#'  print(bm_expand(capital_r, width = 2L, height = 2L),
#'        px = px_ascii)
#' @seealso [bm_extend()] (and [bm_resize()] which makes larger bitmaps
#'           by adding pixels to their sides.
#' @export
bm_expand <- function(bm_object, width = 1L, height = 1L) {
    modify_bm_bitmaps(bm_object, bm_expand_bitmap,
                      width = width, height = height)
}

bm_expand_bitmap <- function(bitmap, width = 1L, height = 1L) {
    if (width != 1L) {
        l <- lapply(seq_len(ncol(bitmap)),
                    function(j) bitmap[, j, drop = FALSE])
        l <- rep(l, each = width)
        bitmap <- do.call(cbind.bm_bitmap, l)
    }
    if (height != 1L) {
        l <- lapply(rev(seq_len(nrow(bitmap))),
                    function(i) bitmap[i, , drop = FALSE])
        l <- rep(l, each = height)
        bitmap <- do.call(rbind.bm_bitmap, l)
    }
    bitmap
}
