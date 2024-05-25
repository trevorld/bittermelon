#' Cast bitmap/pixmap objects to a (normal) matrix
#'
#' `as.matrix.bm_matrix()` casts [bm_bitmap()] objects to an (normal) integer matrix.
#' and [bm_pixmap()] objects to a (normal) character matrix (of color strings).
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
#' @export
as.matrix.bm_matrix <- function(x, ...) {
    class(x) <- NULL
    x
}

#' @rdname as.matrix.bm_matrix
#' @export
as.matrix.bm_matrix <- function(x, ...) {
    class(x) <- NULL
    x
}
