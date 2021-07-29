#' Coerce bitmap glyph objects to matrix
#'
#' `as.matrix.bm_glyph()` coerces `bm_glyph()` objects to an integer matrix.
#'
#' @inheritParams as.character.bm_glyph
#' @return An integer matrix
#' @examples
#'  space_matrix <- matrix(0L, ncol = 8L, nrow = 8L)
#'  space_glyph <- bm_glyph(space_matrix)
#'  print(space_glyph, labels = ".")
#'  print(as.matrix(space_glyph))
#' @export
as.matrix.bm_glyph <- function(x, ...) {
    x <- as_bm_glyph(x)
    class(x) <- matrix(as.integer(x), nrow = nrow(x), ncol = ncol(x))
}
