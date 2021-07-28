#' Bitmap glyph object
#'
#' `bm_glyph()` creates an S3 object representing (monochrome) bitmap (font) glyphs.
#'
#' Bitmap glyphs are represented as integer matrices.
#' The bottom left pixel is represented by the first row and first column.
#' The bottom right pixel is represented by the first row and last column.
#' The top left pixel is represented by the last row and first column.
#' The top right pixel is represented by the last row and last column.
#' @param x Object to be converted to `bm_glyph()`.
#'   If not already an integer matrix it will be cast to one.
#' @return An integer matrix with a \dQuote{bm_glyph} subclass.
#' @examples
#'  space <- bm_glyph(matrix(0, nrow = 16, ncol = 16))
#'  print(space)
#' @seealso [is_bm_glyph()]
#' @export
bm_glyph <- function(x) {
    if (is_bm_glyph(x))
        return(x)
    if (is.matrix(x)) {
        bm_glyph_matrix(x)
    } else {
        stop("Don't know how create 'bm_glyph' from this object")
    }
}

#' Test if the object is a bitmap glyph object
#'
#' `is_bm_glyph()` returns `TRUE` for `bm_glyph` objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @examples
#'  space_matrix <- matrix(0, nrow = 16, ncol = 16)
#'  space_glyph <- bm_glyph(space_matrix)
#'  is_bm_glyph(space_glyph)
#'  is_bm_glyph(space_matrix)
#' @seealso [bm_glyph()]
#' @export
is_bm_glyph <- function(x) inherits(x, "bm_glyph")

bm_glyph_matrix <- function(x) {
    if (!is.integer(x))
        x[, ] <- as.integer(x)
    class(x) <- c("bm_glyph", class(x))
    x
}
