#' Compute "outline" bitmap of a bitmap
#'
#' `bm_outline()` returns a bitmap that is just the \dQuote{outline}
#' of another bitmap.
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @examples
#' square <- bm_bitmap(matrix(1L, nrow = 16L, ncol = 16L))
#' square_outline <- bm_outline(square)
#' print(square_outline)
#'
#' if (require(grid) && capabilities("png")) {
#'   circle <- as_bm_bitmap(circleGrob(), width=16, height=16)
#'   circle_outline <- bm_outline(circle)
#'   print(circle_outline)
#' }
#' @export
bm_outline <- function(x) {
    UseMethod("bm_outline")
}

#' @rdname bm_outline
#' @export
bm_outline.bm_bitmap <- function(x) {
    bm_outline_bitmap(x)
}

#' @rdname bm_outline
#' @export
bm_outline.bm_list <- function(x) {
    bm_lapply(x, bm_outline)
}

bm_outline_bitmap <- function(x) {
    if (nrow(x) <= 2L || ncol(x) <= 2L)
        return(x)

    outline <- x
    for (i in 2:(nrow(x) - 1L)) {
        for (j in 2:(ncol(x) - 1L)) {
            neighbors <- x[i, c(j - 1, j + 1)]
            neighbors <- c(neighbors, x[c(i - 1, i + 1), j])
            if (all(neighbors == 1L))
                outline[i, j] <- 0L
        }
    }
    outline
}
