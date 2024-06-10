#' Bittermelon pixmap matrix object
#'
#' `bm_pixmap()` creates an S3 matrix subclass representing a pixmap.
#'
#' * Intended to represent raster graphic pixmaps especially (but not limited to) pixel art/sprites.
#' * Pixmaps are represented as color string matrices with special class methods.
#' * The bottom left pixel is represented by the first row and first column.
#' * The bottom right pixel is represented by the first row and last column.
#' * The top left pixel is represented by the last row and first column.
#' * The top right pixel is represented by the last row and last column.
#' * Colors are converted to the `"#RRGGBBAA"` color string format.
#' * Fully transparent values like `"transparent"`, `NA`, `"#00000000"` are
#'   all standardized to `"#FFFFFF00"`.
#' * See [bm_bitmap()] for an alternative S3 object backed by a integer matrix.
#'
#' @section Supported S3 methods:
#'
#' * \code{\link{[.bm_bitmap}} and \code{\link{[<-.bm_bitmap}}
#' * [as.matrix.bm_pixmap()]
#' * [as.raster.bm_bitmap()] and [plot.bm_bitmap()]
#' * [format.bm_pixmap()] and [print.bm_pixmap()]
#'
#' @param x Object to be converted to `bm_pixmap()`.
#'   If not already a color string matrix it will be cast to one by [as_bm_pixmap()].
#' @return A character matrix of color strings with a \dQuote{bm_pixmap} subclass.
#' @seealso [as_bm_pixmap()], [is_bm_pixmap()]
#' @examples
#' # Bottom left pixel is **first** row and first column
#' pm <- bm_pixmap(matrix(c("red", "blue", "green", "black"),
#'                 nrow = 2L, byrow = TRUE))
#' plot(pm)
#' @export
bm_pixmap <- function(x) {
    as_bm_pixmap(x)
}
