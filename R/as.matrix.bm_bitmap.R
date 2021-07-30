#' Coerce bitmap objects to matrix
#'
#' `as.matrix.bm_bitmap()` coerces `bm_bitmap()` objects to an integer matrix.
#'
#' @inheritParams as.character.bm_bitmap
#' @return An integer matrix
#' @examples
#'  space_matrix <- matrix(0L, ncol = 8L, nrow = 8L)
#'  space_glyph <- bm_bitmap(space_matrix)
#'  print(space_glyph, labels = ".")
#'  print(as.matrix(space_glyph))
#' @export
as.matrix.bm_bitmap <- function(x, ...) {
    x <- as_bm_bitmap(x)
    class(x) <- NULL
    x
}
