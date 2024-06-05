#' Cast bitmap/pixmap objects to a (normal) matrix
#'
#' `as.matrix.bm_matrix()` casts [bm_bitmap()] objects to a (normal) integer matrix
#' and [bm_pixmap()] objects to a (normal) character matrix (of color strings).
#' Note unless `first_row_is_top = TRUE` the bottom left pixel will still be
#' represented by the pixel in the first row and first column
#' (i.e. these methods simply remove the class names).
#' @param x Either a [bm_bitmap()] or [bm_pixmap()] object.
#' @param first_row_is_top If `TRUE` the first row of the matrix will represent the top of the bitmap
#'                (like [grDevices::as.raster()] objects).
#'                If `FALSE` the first row of the matrix will represent the bottom of the bitmap
#'                (like [bm_bitmap()] and [bm_pixmap()] objects).
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
as.matrix.bm_matrix <- function(x, first_row_is_top = FALSE, ...) {
    class(x) <- NULL
    if (first_row_is_top)
        flip_matrix_vertically(x)
    else
        x
}

#' Cast bitmap/pixmap objects to an array
#'
#' `as.array.bm_bitmap()` / `as.array.bm_pixmap()` casts [bm_bitmap()] / [bm_pixmap()] objects to an array of numeric values representing the RGBA channels.
#' These arrays can be used in functions such as [png::writePNG()].
#' @param x Either a [bm_bitmap()] or [bm_pixmap()] object.
#' @param first_row_is_top If `TRUE` the first row of the matrix will represent the top of the bitmap
#'                (like [grDevices::as.raster()] objects).
#'                If `FALSE` the first row of the matrix will represent the bottom of the bitmap
#'                (like [bm_bitmap()] and [bm_pixmap()] objects).
#' @param ... Currently ignored.
#' @param col Character vector of R color specifications.
#'            First color is used for values equal to 0, second color for values equal to 1, etc.
#' @rdname as.array.bm_matrix
#' @examples
#' corn <- farming_crops_16x16()$corn$portrait
#' a <- as.array(corn)
#' f <- tempfile(fileext = ".png")
#' png::writePNG(a, f)
#' @export
as.array.bm_bitmap <- function(x, ..., first_row_is_top = TRUE,
                               col = getOption("bittermelon.col", col_bitmap)) {
    as.array(as_bm_pixmap(x, col = col), first_row_is_top = first_row_is_top)
}

#' @rdname as.array.bm_matrix
#' @export
as.array.bm_pixmap <- function(x, ..., first_row_is_top = TRUE) {
    m <- as.matrix(x, first_row_is_top = first_row_is_top)
    rgba <- grDevices::col2rgb(as.character(m), alpha = TRUE) / 255
    a <- array(numeric(nrow(x) * ncol(x) * 4L), dim = c(nrow(x), ncol(x), 4L))
    a[seq_len(nrow(x)), seq_len(ncol(x)), 1L] <- rgba[1L, ]
    a[seq_len(nrow(x)), seq_len(ncol(x)), 2L] <- rgba[2L, ]
    a[seq_len(nrow(x)), seq_len(ncol(x)), 3L] <- rgba[3L, ]
    a[seq_len(nrow(x)), seq_len(ncol(x)), 4L] <- rgba[4L, ]
    a
}

flip_matrix_vertically <- function(x) {
    if (nrow(x) > 1L) {
        x[seq.int(nrow(x), 1L, -1L),  , drop = FALSE]
    } else {
        x
    }
}

flip_matrix_horizontally <- function(x) {
    if (ncol(x) > 1L) {
        x[, seq.int(ncol(x), 1L, -1L), drop = FALSE]
    } else {
        x
    }
}
