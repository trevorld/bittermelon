#' Coerce bitmap objects to matrix
#'
#' `as.matrix.bm_bitmap()` coerces `bm_bitmap()` objects to an integer matrix.
#'
#' @inheritParams format.bm_bitmap
#' @return An integer matrix
#' @examples
#'  space_matrix <- matrix(0L, ncol = 8L, nrow = 8L)
#'  space_glyph <- bm_bitmap(space_matrix)
#'  print(space_glyph, px = ".")
#'  print(as.matrix(space_glyph))
#' @export
as.matrix.bm_bitmap <- function(x, ...) {
    class(x) <- NULL
    x
}
