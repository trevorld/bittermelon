#' Extract part of a bitmap
#'
#' `bm_extract()` can be used to extract part of a bitmap.
#' For [bm_bitmap()] and [bm_pixmap()] objects it is a wrapper around [`[`()] with `drop = FALSE` for convenience in pipes.
#'
#' @inheritParams bm_clamp
#' @seealso [`[.bm_matrix`()], [bm_trim()]
#' @export
bm_extract <- function(x, ...) {
	UseMethod("bm_extract")
}

#' @rdname bm_extract
#' @param rows,cols Integer vectors of rows and columns to extract.
#'   Rows are indexed from the **bottom** of the image.
#' @export
bm_extract.bm_matrix <- function(x, rows = seq_len(nrow(x)), cols = seq_len(ncol(x)), ...) {
	x[rows, cols, drop = FALSE]
}

#' @rdname bm_extract
#' @export
bm_extract.bm_list <- function(x, ...) {
	bm_lapply(x, bm_extract, ...)
}

#' @rdname bm_extract
#' @export
`bm_extract.magick-image` <- function(
	x,
	rows = seq_len(bm_heights(x)),
	cols = seq_len(bm_widths(x)),
	...
) {
	pm <- as_bm_pixmap(x)
	pm <- pm[rows, cols, drop = FALSE]
	magick::image_read(pm)
}

#' @rdname bm_extract
#' @export
bm_extract.nativeRaster <- function(x, rows = seq_len(nrow(x)), cols = seq_len(ncol(x)), ...) {
	pm <- as_bm_pixmap.nativeRaster(x)
	pm <- pm[rows, cols, drop = FALSE]
	as.raster(pm, native = TRUE)
}

#' @rdname bm_extract
#' @export
bm_extract.raster <- function(x, rows = seq_len(nrow(x)), cols = seq_len(ncol(x)), ...) {
	pm <- as_bm_pixmap.raster(x)
	pm <- pm[rows, cols, drop = FALSE]
	as.raster(pm, native = FALSE)
}
