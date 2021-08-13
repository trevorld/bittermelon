#' Compute "outline" bitmap of a bitmap
#'
#' `bm_outline()` returns a bitmap that is just the \dQuote{outline}
#' of another bitmap.
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @examples
#'   square <- bm_bitmap(matrix(1L, nrow = 16L, ncol = 16L))
#'   square_outline <- bm_outline(square)
#'   print(square_outline, px = px_ascii)
#'
#'   if (require(grid) && capabilities("png")) {
#'     circle <- as_bm_bitmap(circleGrob(), width=16, height=16)
#'     circle_outline <- bm_outline(circle)
#'     print(circle_outline, px = px_ascii)
#'   }
#' @export
bm_outline <- function(bm_object) {
    modify_bm_bitmaps(bm_object, bm_outline_bitmap)
}

bm_outline_bitmap <- function(bitmap) {
    if (nrow(bitmap) <= 2L || ncol(bitmap) <= 2L)
        return(bitmap)

    outline <- bitmap
    for (i in 2:(nrow(bitmap) - 1L)) {
        for (j in 2:(ncol(bitmap) - 1L)) {
            neighbors <- bitmap[i, c(j - 1, j + 1)]
            neighbors <- c(neighbors, bitmap[c(i - 1, i + 1), j])
            if (all(neighbors == 1L))
                outline[i, j] <- 0L
        }
    }
    outline
}
