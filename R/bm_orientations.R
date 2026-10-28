#' Orientations of bitmaps
#'
#' `bm_orientations()` returns the orientations of the bitmaps.
#' A bitmap is `"landscape"` if it is wider than it is tall,
#' `"portrait"` if it is taller than it is wide, and
#' (by default) `"square"` if it is as wide as it is tall.
#' Use the `square` argument to instead classify square bitmaps as
#' `"portrait"` (the CSS media query convention) or `"landscape"`.
#' `bm_orientations()` is an S3 generic function.
#'
#' @inheritParams bm_clamp
#' @param square The orientation to return for square bitmaps.
#'               Either `"square"` (default), `"portrait"`, or `"landscape"`.
#' @param unique Apply [base::unique()] to the returned character vector.
#' @param ... Ignored.
#' @return A character vector of the orientation of each
#'         of the bitmap objects in `x`.
#'         Each element is either `"landscape"`, `"portrait"`, or `"square"`.
#'         If `unique` is `TRUE` then any duplicates will have been removed.
#' @seealso [bm_widths()] and [bm_heights()].
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz",
#'   package = "bittermelon"
#' )
#' font <- read_hex(font_file)
#' bm_orientations(font) # every glyph is 8 pixels wide and 16 pixels high
#' bm_orientations(bm_rotate(font, 90))
#' corn <- farming_crops_16x16()$corn$portrait
#' bm_orientations(corn) # 16 pixels wide and 16 pixels high
#' bm_orientations(corn, square = "portrait") # CSS media query convention
#' bm_orientations(corn, square = "landscape")
#' @export
bm_orientations <- function(x, square = c("square", "portrait", "landscape"), ...) {
	UseMethod("bm_orientations")
}

#' @rdname bm_orientations
#' @export
bm_orientations.default <- function(x, square = c("square", "portrait", "landscape"), ...) {
	chkDots(...)
	square <- match.arg(square)
	orientations(bm_widths(x), bm_heights(x), square)
}

#' @rdname bm_orientations
#' @export
bm_orientations.bm_list <- function(
	x,
	square = c("square", "portrait", "landscape"),
	unique = TRUE,
	...
) {
	chkDots(...)
	square <- match.arg(square)
	o <- orientations(bm_widths(x, unique = FALSE), bm_heights(x, unique = FALSE), square)
	if (unique) {
		base::unique(o)
	} else {
		o
	}
}

orientations <- function(width, height, square) {
	o <- rep_len(square, length(width))
	names(o) <- names(width)
	o[width > height] <- "landscape"
	o[width < height] <- "portrait"
	o
}
