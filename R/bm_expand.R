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
#'  print(capital_r)
#'  print(bm_expand(capital_r, width = 2L),
#'        px = px_ascii)
#'  print(bm_expand(capital_r, height = 2L),
#'        px = px_ascii)
#'  print(bm_expand(capital_r, width = 2L, height = 2L),
#'        px = px_ascii)
#' @seealso [bm_extend()] (and [bm_resize()] which makes larger bitmaps
#'           by adding pixels to their sides.
#' @export
bm_expand <- function(x, width = 1L, height = 1L) {
    modify_bm_bitmaps(x, bm_expand_bitmap,
                      width = width, height = height)
}

bm_expand_bitmap <- function(x, width = 1L, height = 1L) {
    if (nrow(x) == 0L || ncol(x) == 0L) {
        nr <- height * nrow(x)
        nc <- width * ncol(x)
        return(bm_bitmap(matrix(integer(), nrow = nr, ncol = nc)))
    }
    if (width != 1L) {
        l <- lapply(seq_len(ncol(x)),
                    function(j) x[, j, drop = FALSE])
        l <- rep(l, each = width)
        x <- do.call(cbind.bm_bitmap, l)
    }
    if (height != 1L) {
        l <- lapply(seq.int(nrow(x), 1L, -1L),
                    function(i) x[i, , drop = FALSE])
        l <- rep(l, each = height)
        x <- do.call(rbind.bm_bitmap, l)
    }
    x
}
