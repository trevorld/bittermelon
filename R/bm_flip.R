#' Flip (reflect) bitmaps
#'
#' `bm_flip()` flips (reflects) bitmaps horizontally, vertically, or both.
#' It can flip the entire bitmap or just the glyph in place.
#'
#' @inheritParams bm_clamp
#' @param direction Either "vertical" or "v", "horizontal" or "h",
#'                  OR "both" or "b".
#' @param in_place If `TRUE` flip the glyphs in place (without changing any background padding).
#' @param value Background padding value (to use if `in_place` is `TRUE`)
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#'
#' # Print upside down
#' bml <- as_bm_list("RSTATS", font = font)
#' bml <- bm_flip(bml, "both")
#' bm <- bm_call(bml, cbind, direction = "RTL")
#' print(bm)
#'
#' # Can also modify glyphs "in place"
#' exclamation <- font[[str2ucp("!")]]
#' exclamation_flipped <- bm_flip(exclamation, in_place = TRUE)
#' print(exclamation_flipped)
#'
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' corn_fh <- bm_flip(corn, "h")
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_fh, compress = "v")
#' }
#' @inherit bm_clamp return
#' @export
bm_flip <- function(x, direction = "vertical", in_place = FALSE, value) {
    UseMethod("bm_flip")
}

#' @rdname bm_flip
#' @export
bm_flip.bm_bitmap <- function(x, direction = "vertical", in_place = FALSE, value = 0L) {
    bm_flip_bitmap(x, direction, in_place, value)
}

#' @rdname bm_flip
#' @export
bm_flip.bm_list <- function(x, ...) {
    bm_lapply(x, bm_flip, ...)
}

#' @rdname bm_flip
#' @export
bm_flip.bm_pixmap <- function(x, direction = "vertical", in_place = FALSE, 
                              value = col2hex("transparent")) {
    bm_flip_bitmap(x, direction, in_place, value)
}

#' @rdname bm_flip
#' @export
`bm_flip.magick-image` <- function(x, direction = "vertical", in_place = FALSE, 
                                   value = "transparent") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    bm_flip_bitmap(x, direction, in_place, value)
}

#' @rdname bm_flip
#' @export
bm_flip.nativeRaster <- function(x, direction = "vertical", in_place = FALSE,
                                 value = col2int("transparent")) {
    pm <- as_bm_pixmap(x)
    value <- int2col(as_native(value))
    pm <- bm_flip_bitmap(pm, direction, in_place, value)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_flip
#' @export
bm_flip.raster <- function(x, direction = "vertical", in_place = FALSE, value = "transparent") {
    bm_flip_bitmap(x, direction, in_place, value)
}

bm_flip_bitmap <- function(x, direction, in_place, value) {
    direction <- match.arg(tolower(direction),
                           c("vertical", "v", "horizontal", "h", "both", "b"))
    direction <- substr(direction, 1L, 1L)
    if (in_place) {
        bmpl <- bm_padding_lengths(x, value)
        x <- bm_trim(x, sides = bmpl)
    }
    if (direction %in% c("h", "b")) {
        x <- flip_bitmap_horizontally(x)
    }
    if (direction %in% c("v", "b")) {
        x <- flip_bitmap_vertically(x)
    }
    if (in_place)
        x <- bm_extend(x, value, sides = bmpl)
    x
}

# Doesn't handle nativeRaster... `nara::nr_fliph()`?
flip_bitmap_horizontally <- function(x) {
    if(inherits(x, "magick-image")) {
        magick::image_flop(x)
    } else {
        flip_matrix_horizontally(x)
    }
}

# Doesn't handle nativeRaster... `nara::nr_flipv()`?
flip_bitmap_vertically <- function(x) {
    if(inherits(x, "magick-image")) {
        magick::image_flip(x)
    } else {
        flip_matrix_vertically(x)
    }
}
