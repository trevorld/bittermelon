#' Test if the object is a bitmap object
#'
#' `is_bm_bitmap()` returns `TRUE` for `bm_bitmap` objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @examples
#' space_matrix <- matrix(0L, nrow = 16L, ncol = 16L)
#' is_bm_bitmap(space_matrix)
#' space_glyph <- bm_bitmap(space_matrix)
#' is_bm_bitmap(space_glyph)
#' @seealso [bm_bitmap()]
#' @export
is_bm_bitmap <- function(x) inherits(x, "bm_bitmap")

#' Test if the object is a pixmap object
#'
#' `is_bm_pixmap()` returns `TRUE` for `bm_pixmap` objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @seealso [bm_pixmap()], [as_bm_pixmap()]
#' @examples
#' pm <- bm_pixmap(matrix(c("red", "blue", "green", "black"),
#'                 nrow = 2L, byrow = TRUE))
#' is_bm_pixmap(pm)
#' @export
is_bm_pixmap <- function(x) inherits(x, "bm_pixmap")

is_bm_matrix <- function(x) inherits(x, "bm_matrix")
