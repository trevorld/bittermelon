#' Bitmap object
#'
#' `bm_bitmap()` creates an S3 object representing bitmap.
#'
#' Bitmaps are represented as integer matrices with special class methods.
#' The bottom left pixel is represented by the first row and first column.
#' The bottom right pixel is represented by the first row and last column.
#' The top left pixel is represented by the last row and first column.
#' The top right pixel is represented by the last row and last column.
#' Color bitmaps are supported (the integer can be any non-negative integer)
#' but we are unlikely to ever support exporting color bitmap fonts.
#' Color bitmaps can be cast to black-and-white bitmaps via [bm_clamp()].
#'
#' @section Supported S3 methods:
#'
#' * \code{\link{[.bm_bitmap}} and \code{\link{[<-.bm_bitmap}}
#' * [as.matrix.bm_bitmap()]
#' * [as.raster.bm_bitmap()] and [plot.bm_bitmap()]
#' * [cbind.bm_bitmap()] and [rbind.bm_bitmap()]
#' * [format.bm_bitmap()] and [print.bm_bitmap()]
#' * [Ops.bm_bitmap()] for all the S3 \dQuote{Ops} Group generic functions
#' * [which.bm_bitmap()] (with `which()` re-defined as a generic)
#'
#' @param x Object to be converted to `bm_bitmap()`.
#'   If not already an integer matrix it will be cast to one
#'   by [as_bm_bitmap()].
#' @return An integer matrix with a \dQuote{bm_bitmap} subclass.
#' @examples
#'  space <- bm_bitmap(matrix(0, nrow = 16, ncol = 16))
#'  print(space, px = ".")
#' @seealso [as_bm_bitmap()], [is_bm_bitmap()]
#' @export
bm_bitmap <- function(x) {
    if (is_bm_bitmap(x))
        x
    else
        as_bm_bitmap(x)
}

#' Test if the object is a bitmap glyph object
#'
#' `is_bm_bitmap()` returns `TRUE` for `bm_bitmap` objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @examples
#'  space_matrix <- matrix(0L, nrow = 16L, ncol = 16L)
#'  is_bm_bitmap(space_matrix)
#'  space_glyph <- bm_bitmap(space_matrix)
#'  is_bm_bitmap(space_glyph)
#' @seealso [bm_bitmap()]
#' @export
is_bm_bitmap <- function(x) inherits(x, "bm_bitmap")
