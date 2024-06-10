#' Rotate bitmaps 0, 90, 180, or 270 degrees
#'
#' `bm_rotate()` losslessly rotates bitmaps by 0, 90, 180, or 270 degrees.
#' If `90` or `270` degrees are indicated the width and height of the bitmap will be flipped.
#'
#' @inheritParams bm_clamp
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
#'          [bm_flip()] with `direction` "both" and `in_place` `TRUE` can
#'          rotate glyphs 180 degrees in place.
#' @inherit bm_clamp return
#' @export
bm_rotate <- function(x, angle = 0L, clockwise = TRUE) {
    UseMethod("bm_rotate")
}

#' @rdname bm_rotate
#' @export
bm_rotate.bm_matrix <- function(x, angle = 0L, clockwise = TRUE) {
    bm_rotate_bitmap(x, angle, clockwise)
}

#' @rdname bm_rotate
#' @export
bm_rotate.nativeRaster <- function(x, angle = 0L, clockwise = TRUE) {
    pm <- as_bm_pixmap.nativeRaster(x)
    pm <- bm_rotate_bitmap(pm, angle, clockwise)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_rotate
#' @export
`bm_rotate.magick-image` <- function(x, angle = 0L, clockwise = TRUE) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    if (!clockwise)
        angle <- -angle
    magick::image_rotate(x, angle)
}

#' @rdname bm_rotate
#' @export
bm_rotate.raster <- function(x, angle = 0L, clockwise = TRUE) {
    as.raster(bm_rotate_bitmap(x, angle, !clockwise))
}

#' @rdname bm_rotate
#' @export
bm_rotate.bm_list <- function(x, ...) {
    bm_lapply(x, bm_rotate, ...)
}

bm_rotate_bitmap <- function(x, angle = 0, clockwise = TRUE) {
    angle <- as.integer(angle)
    if (clockwise)
        angle <- -angle
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
