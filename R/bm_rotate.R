#' Rotate bitmaps 0, 90, 180, or 270 degrees
#'
#' `bm_rotate()` losslessly rotates bitmaps by 0, 90, 180, or 270 degrees.
#' If `90` or `270` degrees are indicated the width and height of the bitmap will be flipped
#' unless `in_place` is `TRUE`.
#'
#' @inheritParams bm_flip
#' @inheritParams bm_clamp
#' @param in_place If `TRUE` rotate the glyphs in place (without changing any background padding).
#' @param angle Angle to rotate bitmap by.
#' @param clockwise If `TRUE` rotate bitmaps clockwise.
#'                  Note Unicode's convention is to rotate glyphs clockwise
#'                  i.e. the top of the "BLACK CHESS PAWN ROTATED NINETY DEGREES" glyph points right.
#' @examples
#' # as_bm_list.character()
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(bm_rotate(capital_r, 90))
#' print(bm_rotate(capital_r, 180))
#' print(bm_rotate(capital_r, 270))
#' print(bm_rotate(capital_r, 90, clockwise = FALSE))
#'
#' corn <- farming_crops_16x16()$corn$portrait
#' corn_180 <- bm_rotate(corn, 180)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_180, compress = "v")
#' }
#' @seealso [bm_distort()] can do other (distorted) rotations by careful
#'          use of its `vp` [grid::viewport()] argument.
#' @inherit bm_clamp return
#' @export
bm_rotate <- function(x, angle = 0L, clockwise = TRUE, in_place = FALSE, value) {
	UseMethod("bm_rotate")
}

#' @rdname bm_rotate
#' @export
bm_rotate.bm_bitmap <- function(x, angle = 0L, clockwise = TRUE, in_place = FALSE, value = 0L) {
	if (in_place) {
		orig_w <- ncol(x)
		orig_h <- nrow(x)
		bmpl <- bm_padding_lengths(x, value)
		x <- bm_trim(x, sides = bmpl)
	}
	x <- bm_rotate_bitmap(x, angle, clockwise)
	if (in_place) {
		new_bmpl <- bm_in_place_padding(orig_w, orig_h, bmpl, ncol(x), nrow(x))
		x <- bm_extend(x, value, sides = new_bmpl)
	}
	x
}

#' @rdname bm_rotate
#' @export
bm_rotate.bm_list <- function(x, ...) {
	bm_lapply(x, bm_rotate, ...)
}

#' @rdname bm_rotate
#' @export
bm_rotate.bm_pixmap <- function(
	x,
	angle = 0L,
	clockwise = TRUE,
	in_place = FALSE,
	value = col2hex("transparent")
) {
	if (in_place) {
		orig_w <- ncol(x)
		orig_h <- nrow(x)
		bmpl <- bm_padding_lengths(x, value)
		x <- bm_trim(x, sides = bmpl)
	}
	x <- bm_rotate_bitmap(x, angle, clockwise)
	if (in_place) {
		new_bmpl <- bm_in_place_padding(orig_w, orig_h, bmpl, ncol(x), nrow(x))
		x <- bm_extend(x, value, sides = new_bmpl)
	}
	x
}

#' @rdname bm_rotate
#' @export
`bm_rotate.magick-image` <- function(
	x,
	angle = 0L,
	clockwise = TRUE,
	in_place = FALSE,
	value = "transparent"
) {
	stopifnot(requireNamespace("magick", quietly = TRUE))
	if (in_place) {
		pm <- `as_bm_pixmap.magick-image`(x)
		value <- col2hex(value)
		pm <- bm_rotate.bm_pixmap(
			pm,
			angle = angle,
			clockwise = clockwise,
			in_place = TRUE,
			value = value
		)
		magick::image_read(pm)
	} else {
		if (!clockwise) {
			angle <- -angle
		}
		magick::image_rotate(x, angle)
	}
}

#' @rdname bm_rotate
#' @export
bm_rotate.nativeRaster <- function(
	x,
	angle = 0L,
	clockwise = TRUE,
	in_place = FALSE,
	value = col2int("transparent")
) {
	pm <- as_bm_pixmap(x)
	value <- int2col(as_native(value))
	pm <- bm_rotate.bm_pixmap(
		pm,
		angle = angle,
		clockwise = clockwise,
		in_place = in_place,
		value = value
	)
	as.raster(pm, native = TRUE)
}

#' @rdname bm_rotate
#' @export
bm_rotate.raster <- function(
	x,
	angle = 0L,
	clockwise = TRUE,
	in_place = FALSE,
	value = "transparent"
) {
	if (in_place) {
		pm <- as_bm_pixmap(x)
		value <- col2hex(value)
		pm <- bm_rotate.bm_pixmap(
			pm,
			angle = angle,
			clockwise = clockwise,
			in_place = TRUE,
			value = value
		)
		as.raster(pm)
	} else {
		as.raster(bm_rotate_bitmap(x, angle, !clockwise))
	}
}

bm_in_place_padding <- function(orig_w, orig_h, bmpl, rot_w, rot_h) {
	glyph_w <- orig_w - bmpl[["left"]] - bmpl[["right"]]
	glyph_h <- orig_h - bmpl[["top"]] - bmpl[["bottom"]]
	cx <- bmpl[["left"]] + (glyph_w - 1L) %/% 2L
	cy <- bmpl[["top"]] + (glyph_h - 1L) %/% 2L
	new_left <- cx - (rot_w - 1L) %/% 2L
	new_top <- cy - (rot_h - 1L) %/% 2L
	new_right <- orig_w - new_left - rot_w
	new_bottom <- orig_h - new_top - rot_h
	new_bmpl <- c(top = new_top, right = new_right, bottom = new_bottom, left = new_left)
	if (any(new_bmpl < 0L)) {
		cli::cli_abort(
			"The rotated glyph ({rot_w}x{rot_h}) cannot fit within the original dimensions ({orig_w}x{orig_h})."
		)
	}
	new_bmpl
}

bm_rotate_bitmap <- function(x, angle = 0, clockwise = TRUE) {
	angle <- as.integer(angle)
	if (clockwise) {
		angle <- -angle
	}
	angle <- angle %% 360L
	stopifnot(angle %in% c(0L, 90L, 180L, 270L))
	if (angle == 90L) {
		x <- flip_matrix_horizontally(t(x))
	} else if (angle == 180L) {
		x <- flip_matrix_horizontally(flip_matrix_vertically(x))
	} else if (angle == 270L) {
		x <- flip_matrix_vertically(t(x))
	} # No change if angle == 0L
	x
}
