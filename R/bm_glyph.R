#' Bitmap glyph object
#'
#' `bm_glyph()` creates an S3 object representing bitmap glyphs.
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
#'  print(space, labels = ".")
#' @seealso [as_bm_glyph()], [is_bm_glyph(), [as.character.bm_glyph()], [print.bm_glyph()]
#' @export
bm_glyph <- function(x) {
    if (is_bm_glyph(x))
        x
    else
        as_bm_glyph(x)
}

#' Test if the object is a bitmap glyph object
#'
#' `is_bm_glyph()` returns `TRUE` for `bm_glyph` objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @examples
#'  space_matrix <- matrix(0L, nrow = 16L, ncol = 16L)
#'  is_bm_glyph(space_matrix)
#'  space_glyph <- bm_glyph(space_matrix)
#'  is_bm_glyph(space_glyph)
#' @seealso [bm_glyph()]
#' @export
is_bm_glyph <- function(x) inherits(x, "bm_glyph")

#' Coerce to bitmap glyph objects
#'
#' `as_bm_glyph()` turns an existing object into a `bm_glyph()` object.
#'
#' @param x An object that can reasonably be coerced to a `bm_glyph()` object.
#' @param ... Further arguments passed to or from other methods.
#' @return A `bm_glyph()` object.
#' @examples
#'  space_matrix <- matrix(0L, nrow = 16L, ncol = 16L)
#'  space_glyph <- as_bm_glyph(space_matrix)
#'  is_bm_glyph(space_glyph)
#' @seealso [bm_glyph()]
#' @export
as_bm_glyph <- function(x, ...) {
    UseMethod("as_bm_glyph")
}

#' @rdname as_bm_glyph
#' @export
as_bm_glyph.matrix <- function(x, ...) {
    if (!is.integer(x)) {
        x[, ] <- suppressWarnings(as.integer(x))
    }
    stopifnot(!any(is.na(x)))
    class(x) <- c("bm_glyph", class(x))
    x
}

#' @rdname as_bm_glyph
#' @export
as_bm_glyph.default <- function(x, ...) {
    as_bm_glyph.matrix(as.matrix(x))
}
