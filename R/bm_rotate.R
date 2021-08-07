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
#'   # as_bm_list.character()
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   capital_r <- font[[str2ucp("R")]]
#'   print(bm_rotate(capital_r, 90), px = px_ascii)
#'   print(bm_rotate(capital_r, 180), px = px_ascii)
#'   print(bm_rotate(capital_r, 270), px = px_ascii)
#'   print(bm_rotate(capital_r, 90, clockwise = FALSE), px = px_ascii)
#'
#' @seealso [bm_distort()] can do other (distorted) rotations by careful
#'          use of its `vp` [grid::viewport()] argument.
#'          [bm_flip()] with `direction` "both" and `in_place` `TRUE` can
#'          rotate glyphs 180 degrees in place.
#' @inherit bm_clamp return
#' @export
bm_rotate <- function(bm_object, angle = 0, clockwise = TRUE) {
    modify_bm_bitmaps(bm_object, bm_rotate_bitmap,
                      angle = angle, clockwise = clockwise)
}

bm_rotate_bitmap <- function(bitmap, angle = 0, clockwise = TRUE) {
    angle <- as.integer(angle)
    if (clockwise)
        angle <- -angle
    angle <- angle %% 360L
    stopifnot(angle %in% c(0L, 90L, 180L, 270L))
    if (angle == 90L) {
        bitmap <- bm_flip(t(bitmap), direction = "horizontal")
    } else if (angle == 180L) {
        bitmap <- bm_flip(bitmap, direction = "both")
    } else if (angle == 270L) {
        bitmap <- bm_flip(t(bitmap), direction = "vertical")
    } # No change if angle == 0L
    bitmap
}
