#' Shift elements within bitmaps
#'
#' Shifts non-padding elements within bitmaps by trimming on a specified side and padding on the other
#' while preserving the width and height of the original bitmap.
#'
#' This function is a convenience wrapper around [bm_trim()] and [bm_extend()].
#'
#' @inheritParams bm_extend
#' @param top Number of pixels to shift towards the top side.
#' @param right Number of pixels to shift towards the right side.
#' @param bottom Number of pixels to shift towards the bottom side.
#' @param left Number of pixels to shift towards the left side.
#' @param overflow How to handle shifts that would push content off the bitmap.
#'                 One of `"clip"`, `"error"`, or `"wrap"`.
#'                 If `"clip"` (the default) then content that is pushed off the bitmap is silently lost.
#'                 If `"error"` then an error is thrown if the shift would clip any non-padding content.
#'                 If `"wrap"` then content that is pushed off one side of the bitmap
#'                 wraps around to the other side.
#' @inherit bm_clamp return
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(capital_r)
#' capital_r <- bm_shift(capital_r, bottom = 2L, right = 1L)
#' print(capital_r)
#' corn <- farming_crops_16x16()$corn$portrait
#' print(bm_padding_lengths(corn))
#' corn_shifted <- bm_shift(corn, left = 1L, top = 2L)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_shifted, bg = "cyan", compress = "v")
#' }
#' corn_wrapped <- bm_shift(corn, right = 4L, overflow = "wrap")
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_wrapped, bg = "cyan", compress = "v")
#' }
#' @seealso [bm_trim()] and [bm_extend()]
#' @export
bm_shift <- function(
	x,
	value,
	top = NULL,
	right = NULL,
	bottom = NULL,
	left = NULL,
	overflow = "clip"
) {
	stopifnot(is.null(top) || is.null(bottom))
	stopifnot(is.null(left) || is.null(right))
	UseMethod("bm_shift")
}

#' @rdname bm_shift
#' @export
bm_shift.bm_bitmap <- function(
	x,
	value = 0L,
	top = NULL,
	right = NULL,
	bottom = NULL,
	left = NULL,
	overflow = "clip"
) {
	bm_shift_bitmap(
		x,
		value = value,
		top = top,
		right = right,
		bottom = bottom,
		left = left,
		overflow = overflow
	)
}

#' @rdname bm_shift
#' @export
bm_shift.bm_list <- function(x, ...) {
	bm_lapply(x, bm_shift, ...)
}

#' @rdname bm_shift
#' @export
bm_shift.bm_pixmap <- function(
	x,
	value = col2hex("transparent"),
	top = NULL,
	right = NULL,
	bottom = NULL,
	left = NULL,
	overflow = "clip"
) {
	bm_shift_bitmap(
		x,
		value = value,
		top = top,
		right = right,
		bottom = bottom,
		left = left,
		overflow = overflow
	)
}

#' @rdname bm_shift
#' @export
`bm_shift.magick-image` <- function(
	x,
	value = "transparent",
	top = NULL,
	right = NULL,
	bottom = NULL,
	left = NULL,
	overflow = "clip"
) {
	stopifnot(requireNamespace("magick", quietly = TRUE))
	pm <- bm_shift_bitmap(
		as_bm_pixmap(x),
		value = col2hex(value),
		top = top,
		right = right,
		bottom = bottom,
		left = left,
		overflow = overflow
	)
	magick::image_read(pm)
}

#' @rdname bm_shift
#' @export
bm_shift.nativeRaster <- function(
	x,
	value = col2int("transparent"),
	top = NULL,
	right = NULL,
	bottom = NULL,
	left = NULL,
	overflow = "clip"
) {
	pm <- bm_shift_bitmap(
		as_bm_pixmap(x),
		value = int2col(as_native(value)),
		top = top,
		right = right,
		bottom = bottom,
		left = left,
		overflow = overflow
	)
	as.raster(pm, native = TRUE)
}

#' @rdname bm_shift
#' @export
bm_shift.raster <- function(
	x,
	value = "transparent",
	top = NULL,
	right = NULL,
	bottom = NULL,
	left = NULL,
	overflow = "clip"
) {
	pm <- bm_shift_bitmap(
		as_bm_pixmap(x),
		value = col2hex(value),
		top = top,
		right = right,
		bottom = bottom,
		left = left,
		overflow = overflow
	)
	as.raster(pm)
}

bm_shift_bitmap <- function(
	x,
	value = 0L,
	top = NULL,
	right = NULL,
	bottom = NULL,
	left = NULL,
	overflow = "clip"
) {
	overflow <- match.arg(overflow, c("clip", "error", "wrap"))
	n_top <- top %||% 0L
	n_right <- right %||% 0L
	n_bottom <- bottom %||% 0L
	n_left <- left %||% 0L

	nr <- nrow(x)
	nc <- ncol(x)
	stopifnot(n_top + n_bottom <= nr)
	stopifnot(n_left + n_right <= nc)

	if (overflow == "wrap") {
		if (n_top > 0L) {
			x <- x[c(seq.int(nr - n_top + 1L, nr), seq.int(1L, nr - n_top)), , drop = FALSE]
		}
		if (n_bottom > 0L) {
			x <- x[c(seq.int(n_bottom + 1L, nr), seq.int(1L, n_bottom)), , drop = FALSE]
		}
		if (n_right > 0L) {
			x <- x[, c(seq.int(nc - n_right + 1L, nc), seq.int(1L, nc - n_right)), drop = FALSE]
		}
		if (n_left > 0L) {
			x <- x[, c(seq.int(n_left + 1L, nc), seq.int(1L, n_left)), drop = FALSE]
		}
	} else {
		if (overflow == "error") {
			pl <- bm_padding_lengths(x, value)
			if (n_top > pl[["top"]]) {
				cli::cli_abort(c(
					"Shifting top by {n_top} would clip non-padding content.",
					i = "Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around."
				))
			}
			if (n_right > pl[["right"]]) {
				cli::cli_abort(c(
					"Shifting right by {n_right} would clip non-padding content.",
					i = "Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around."
				))
			}
			if (n_bottom > pl[["bottom"]]) {
				cli::cli_abort(c(
					"Shifting bottom by {n_bottom} would clip non-padding content.",
					i = "Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around."
				))
			}
			if (n_left > pl[["left"]]) {
				cli::cli_abort(c(
					"Shifting left by {n_left} would clip non-padding content.",
					i = "Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around."
				))
			}
		}
		x <- bm_trim(x, top = top, right = right, bottom = bottom, left = left)
		x <- bm_extend(x, value = value, top = bottom, right = left, bottom = top, left = right)
	}
	x
}
