#' Flip (reflect) bitmaps
#'
#' `bm_flip()` flips (reflects) bitmaps horizontally, vertically, or both.
#' It can flip the entire bitmap or just the glyph in place.
#'
#' @inheritParams bm_clamp
#' @param direction Either "vertical" or "v", "horizontal" or "h",
#'                  OR "both" or "b".
#' @param in_place If `TRUE` flip the glyphs in place (without changing any white space padding).
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'
#'   # Print upside down
#'   bml <- as_bm_list("RSTATS", font = font)
#'   bml <- bm_flip(bml, "both")
#'   bm <- bm_call(bml, cbind, direction = "RTL")
#'   print(bm, px = px_ascii)
#'
#'   # Can also modify glyphs "in place"
#'   exclamation <- font[[str2ucp("!")]]
#'   exclamation_flipped <- bm_flip(exclamation, in_place = TRUE)
#'   print(exclamation_flipped, px = px_ascii)
#' @inherit bm_clamp return
#' @export
bm_flip <- function(bm_object, direction = "vertical", in_place = FALSE) {
    direction <- match.arg(tolower(direction),
                           c("vertical", "v", "horizontal", "h", "both", "b"))
    direction <- substr(direction, 1L, 1L)
    modify_bm_bitmaps(bm_object, bm_flip_bitmap,
                      direction = direction, in_place = in_place)
}

bm_flip_bitmap <- function(bitmap, direction = "v", in_place = in_place) {
    if (in_place) {
        bmpl <- bm_padding_lengths(bitmap)
        bitmap <- bm_trim(bitmap, sides = bmpl)
    }
    if (direction %in% c("h", "b")) {
        for (i in seq_len(nrow(bitmap)))
            bitmap[i, ] <- rev(bitmap[i, ])
    }
    if (direction %in% c("v", "b")) {
        for (j in seq_len(ncol(bitmap)))
            bitmap[, j] <- rev(bitmap[, j])
    }
    if (in_place)
        bitmap <- bm_extend(bitmap, sides = bmpl)
    bitmap
}
