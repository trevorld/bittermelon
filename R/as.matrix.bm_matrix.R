#' Cast bitmap/pixmap objects to a (normal) matrix
#'
#' `as.matrix.bm_matrix()` casts [bm_bitmap()] objects to a (normal) integer matrix.
#' and [bm_pixmap()] objects to a (normal) character matrix (of color strings).
#' Note the bottom left pixel will still represented by the first row and first column,
#' these methods simply remove the class names.
#'
#' @param x Either a [bm_bitmap()] or [bm_pixmap()] object.
#' @param ... Currently ignored.
#' @return Either an integer matrix if `x` is a [bm_bitmap()] object
#'         or a character matrix if `x` is a [bm_pixmap()] object.
#' @examples
#'  space_matrix <- matrix(0L, ncol = 8L, nrow = 8L)
#'  space_glyph <- bm_bitmap(space_matrix)
#'  print(space_glyph, px = ".")
#'  print(as.matrix(space_glyph))
#' @rdname as.matrix.bm_matrix
#' @aliases as.matrix.bm_bitmap as.matrix.bm_pixmap
#' @param first_row_is_top If `TRUE` the first row of the matrix will represent the top of the bitmap
#'                (like [grDevices::as.raster()] objects).
#'                If `FALSE` the first row of the matrix will represent the bottom of the bitmap
#'                (like [bm_bitmap()] and [bm_pixmap()] objects).
#' @export
as.matrix.bm_matrix <- function(x, first_row_is_top = FALSE, ...) {
    class(x) <- NULL
    if (first_row_is_top)
        flip_matrix_vertically(x)
    else
        x
}

flip_matrix_vertically <- function(x) {
    if (nrow(x) > 1L) {
        x[seq.int(nrow(x), 1L, -1L),  , drop = FALSE]
    } else {
        x
    }
}

flip_matrix_horizontally <- function(x) {
    if (ncol(x) > 1L) {
        x[, seq.int(ncol(x), 1L, -1L), drop = FALSE]
    } else {
        x
    }
}
