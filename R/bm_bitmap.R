#' Bittermelon bitmap matrix object
#'
#' `bm_bitmap()` creates an S3 matrix subclass representing a bitmap.
#'
#' * Intended to represent binary bitmaps especially (but not limited to) bitmap font glyphs.
#' * Bitmaps are represented as integer matrices with special class methods.
#' * The bottom left pixel is represented by the first row and first column.
#' * The bottom right pixel is represented by the first row and last column.
#' * The top left pixel is represented by the last row and first column.
#' * The top right pixel is represented by the last row and last column.
#' * Non-binary bitmaps are supported (the integer can be any non-negative integer)
#'   but we are unlikely to ever support exporting color bitmap fonts.
#' * Non-binary bitmaps can be cast to binary bitmaps via [bm_clamp()].
#' * See [bm_pixmap()] for an alternative S3 object backed by a color string matrix.
#'
#' @section Supported S3 methods:
#'
#' * \code{\link{[.bm_bitmap}} and \code{\link{[<-.bm_bitmap}}
#' * [as.matrix.bm_bitmap()]
#' * [as.raster.bm_bitmap()] and [plot.bm_bitmap()]
#' * [cbind.bm_bitmap()] and [rbind.bm_bitmap()]
#' * [format.bm_bitmap()] and [print.bm_bitmap()]
#' * [Ops.bm_bitmap()] for all the S3 \dQuote{Ops} Group generic functions
#'
#' @param x Object to be converted to `bm_bitmap()`.
#'   If not already an integer matrix it will be cast to one
#'   by [as_bm_bitmap()].
#' @return An integer matrix with \dQuote{bm_bitmap} and \dQuote{bm_matrix} subclasses.
#' @examples
#'  space <- bm_bitmap(matrix(0, nrow = 16, ncol = 16))
#'  print(space)
#' @seealso [as_bm_bitmap()], [is_bm_bitmap()]
#' @export
bm_bitmap <- function(x) {
    as_bm_bitmap(x)
}
