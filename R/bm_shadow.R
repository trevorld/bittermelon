#' Bitmap shadow, bold, and glow effects
#'
#' `bm_shadow()` adds a basic "shadow" effect to the bitmap(s).
#' `bm_bold()` is a variant with different defaults to create a basic "bold" effect.
#' `bm_glow()` adds a basic "glow" effect to the bitmap(s).
#'
#' @inheritParams bm_clamp
#' @param value The integer value for the shadow, bold, or glow effect.
#' @param top How many pixels above should the shadow go.
#' @param right How many pixels right should the shadow go.
#'              if `top`, `right`, `bottom`, and `left` are all `NULL` then defaults to `1L`.
#' @param bottom How many pixels below should the shadow go.
#'              if `top`, `right`, `bottom`, and `left` are all `NULL`
#'              then defaults to `1L` for `bm_shadow()` and `0L` for `bm_embolden()`.
#' @param left How many pixels left should the shadow go.
#' @param extend Make the bitmap larger to give the new glyph more "room".
#' @param corner Fill in the corners.
#' @param bg Bitmap background value.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(capital_r)
#' print(bm_shadow(capital_r))
#' print(bm_bold(capital_r))
#' print(bm_glow(capital_r))
#' print(bm_glow(capital_r, corner = TRUE))
#'
#' corn <- farming_crops_16x16()$corn$portrait
#' corn_shadow <- bm_shadow(corn, "red")
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_shadow, compress = "v")
#' }
#'
#' corn_glow <- bm_glow(corn, "cyan", corner = TRUE)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_glow, compress = "v")
#' }
#' @inherit bm_clamp return
#' @seealso [bm_extend()] and [bm_shift()]
#' @export
bm_shadow <- function(x, value,
                      top = NULL, right = NULL, bottom = NULL, left = NULL,
                      extend = TRUE, bg) {
    stopifnot(is.null(top) || is.null(bottom))
    stopifnot(is.null(left) || is.null(right))
    UseMethod("bm_shadow")
}

#' @rdname bm_shadow
#' @export
bm_shadow.bm_bitmap <- function(x, value = 2L,
                                top = NULL, right = NULL, bottom = NULL, left = NULL,
                                extend = TRUE, bg = 0L) {
    bm_shadow_bitmap(x, value = as.integer(value), bg = as.integer(bg),
                     top = top, right = right, bottom = bottom, left = left, extend = extend)
}

#' @rdname bm_shadow
#' @export
bm_shadow.bm_list <- function(x, ...) {
    bm_lapply(x, bm_shadow, ...)
}

#' @rdname bm_shadow
#' @export
bm_shadow.bm_pixmap <- function(x, value = col2hex("black"),
                                top = NULL, right = NULL, bottom = NULL, left = NULL,
                                extend = TRUE, bg = col2hex("transparent")) {
    bm_shadow_bitmap(x, value = col2hex(value), bg = col2hex(bg),
                     top = top, right = right, bottom = bottom, left = left,
                     extend = extend)
}

#' @rdname bm_shadow
#' @export
`bm_shadow.magick-image` <- function(x, value = "black",
                                top = NULL, right = NULL, bottom = NULL, left = NULL,
                                extend = TRUE, bg = "transparent") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    pm <- bm_shadow_bitmap(as_bm_pixmap(x), value = col2hex(value), bg = col2hex(bg),
                           top = top, right = right, bottom = bottom, left = left,
                           extend = extend)
    magick::image_read(pm)
}

#' @rdname bm_shadow
#' @export
bm_shadow.nativeRaster <- function(x, value = col2int("black"),
                                top = NULL, right = NULL, bottom = NULL, left = NULL,
                                extend = TRUE, bg = "transparent") {
    pm <- bm_shadow_bitmap(as_bm_pixmap(x),
                           value = int2col(as_native(value)), bg = int2col(as_native(bg)),
                           top = top, right = right, bottom = bottom, left = left,
                           extend = extend)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_shadow
#' @export
bm_shadow.raster <- function(x, value = "black",
                             top = NULL, right = NULL, bottom = NULL, left = NULL,
                             extend = TRUE, bg = "transparent") {
    pm <- bm_shadow_bitmap(as_bm_pixmap(x), value = col2hex(value), bg = col2hex(bg),
                           top = top, right = right, bottom = bottom, left = left, extend = extend)
    as.raster(pm)
}

bm_shadow_bitmap <- function(x, value, bg,
                              top = NULL, right = NULL, bottom = NULL, left = NULL,
                              extend = TRUE) {
    if (is.null(top) && is.null(right) && is.null(bottom) && is.null(left)) {
        right <- 1L
        bottom <- 1L
    }
    if (extend)
        x <- bm_extend(x, value = bg, 
                       top = top, right = right, bottom = bottom, left = left)
    shadow <- bm_shift(x, value = bg, top = top, right = right, bottom = bottom, left = left)
    shadow[which(as.logical(shadow != bg))] <- value
    bm_overlay(shadow, x)
}

#' @rdname bm_shadow
#' @export
bm_bold <- function(x, value = 1L,
                    top = NULL, right = NULL, bottom = NULL, left = NULL,
                    extend = TRUE) {
    UseMethod("bm_bold")
}

#' @rdname bm_shadow
#' @export
bm_bold.bm_bitmap <- function(x, value = 1L,
                              top = NULL, right = NULL, bottom = NULL, left = NULL,
                              extend = TRUE) {
    bm_bold_bitmap(x, value = as.integer(value),
                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                   extend = TRUE)
}

#' @rdname bm_shadow
#' @export
bm_bold.bm_list <- function(x, ...) {
    bm_lapply(x, bm_bold, ...)
}

#' @rdname bm_shadow
#' @export
bm_bold.bm_pixmap <- function(x, value = col2hex("black"),
                              top = NULL, right = NULL, bottom = NULL, left = NULL,
                              extend = TRUE) {
    bm_bold_bitmap(x, value = col2hex(value),
                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                   extend = TRUE)
}

#' @rdname bm_shadow
#' @export
`bm_bold.magick-image` <- function(x, value = "black",
                                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                                   extend = TRUE) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    pm <- bm_bold_bitmap(as_bm_pixmap(x), value = col2hex(value),
                         top = NULL, right = NULL, bottom = NULL, left = NULL,
                         extend = TRUE)
    magick::image_read(pm)
}

#' @rdname bm_shadow
#' @export
bm_bold.nativeRaster <- function(x, value = col2int("black"),
                           top = NULL, right = NULL, bottom = NULL, left = NULL,
                           extend = TRUE) {
    pm <- bm_bold_bitmap(as_bm_pixmap(x), value = int2col(as_native(value)),
                         top = NULL, right = NULL, bottom = NULL, left = NULL,
                         extend = TRUE)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_shadow
#' @export
bm_bold.raster <- function(x, value = "black",
                           top = NULL, right = NULL, bottom = NULL, left = NULL,
                           extend = TRUE) {
    pm <- bm_bold_bitmap(as_bm_pixmap(x), value = col2hex(value),
                         top = NULL, right = NULL, bottom = NULL, left = NULL,
                         extend = TRUE)
    as.raster(pm)
}

bm_bold_bitmap <- function(x, value,
                           top = NULL, right = NULL, bottom = NULL, left = NULL,
                           extend = TRUE) {
    stopifnot(is.null(top) || is.null(bottom))
    stopifnot(is.null(left) || is.null(right))
    if (is.null(top) && is.null(right) && is.null(bottom) && is.null(left)) {
        right <- 1L
    }
    bm_shadow(x, value = value,
              top = top, right = right, bottom = bottom, left = left, extend = extend)
}

#' @rdname bm_shadow
#' @export
bm_glow <- function(x, value, extend = TRUE, corner = FALSE, bg) {
    UseMethod("bm_glow")
}

#' @rdname bm_shadow
#' @export
bm_glow.bm_bitmap <- function(x, value = 2L, extend = TRUE, corner = FALSE, bg = 0L) {
    bm_glow_bitmap(x, value = as.integer(value), bg = as.integer(bg),
                   extend = extend, corner = corner)
}

#' @rdname bm_shadow
#' @export
bm_glow.bm_list <- function(x, ...) {
    bm_lapply(x, bm_glow, ...)
}

#' @rdname bm_shadow
#' @export
bm_glow.bm_pixmap <- function(x, value = col2hex("black"), extend = TRUE, corner = FALSE, 
                              bg = col2hex("transparent")) {
    bm_glow_bitmap(x, value = col2hex(value), bg = col2hex(bg),
                   extend = extend, corner = corner)
}

#' @rdname bm_shadow
#' @export
`bm_glow.magick-image` <- function(x, value = "black", extend = TRUE, corner = FALSE, 
                              bg = "transparent") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    pm <- bm_glow_bitmap(as_bm_pixmap(x), value = col2hex(value), bg = col2hex(bg),
                         extend = extend, corner = corner)
    magick::image_read(pm)
}

#' @rdname bm_shadow
#' @export
bm_glow.nativeRaster <- function(x, value = "black", extend = TRUE, corner = FALSE, 
                              bg = "transparent") {
    pm <- bm_glow_bitmap(as_bm_pixmap(x),
                         value = int2col(as_native(value)), 
                         bg = int2col(as_native(bg)),
                         extend = extend, corner = corner)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_shadow
#' @export
bm_glow.raster <- function(x, value = "black", extend = TRUE, corner = FALSE, 
                              bg = "transparent") {
    pm <- bm_glow_bitmap(as_bm_pixmap(x), value = col2hex(value), bg = col2hex(bg),
                         extend = extend, corner = corner)
    as.raster(pm)
}

bm_glow_bitmap <- function(x, value = 2L, bg = 0L, extend = TRUE, corner = FALSE) {
    bmt <- bm_shadow(x, value = value, bg = bg, extend = extend, top = 1L)
    bmr <- bm_shadow(x, value = value, bg = bg, extend = extend, right = 1L)
    bmb <- bm_shadow(x, value = value, bg = bg, extend = extend, bottom = 1L)
    bml <- bm_shadow(x, value = value, bg = bg, extend = extend, left = 1L)
    if (extend) {
        bm_new <- bm_extend(x, value = bg, sides = 1L)
        bmt <- bm_extend(bmt, value = bg, right = 1L, bottom = 1L, left = 1L)
        bmr <- bm_extend(bmr, value = bg, top = 1L, bottom = 1L, left = 1L)
        bmb <- bm_extend(bmb, value = bg, top = 1L, right = 1L, left = 1L)
        bml <- bm_extend(bml, value = bg, top = 1L, right = 1L, bottom = 1L)
    } else {
        bm_new <- x
    }
    bm_new <- bm_overlay(bm_new, under = bmt, bg = bg)
    bm_new <- bm_overlay(bm_new, under = bmr, bg = bg)
    bm_new <- bm_overlay(bm_new, under = bmb, bg = bg)
    bm_new <- bm_overlay(bm_new, under = bml, bg = bg)
    if (corner) {
        bmtr <- bm_shadow(x, value = value, bg = bg, extend = extend, top = 1L, right = 1L)
        bmbr <- bm_shadow(x, value = value, bg = bg, extend = extend, bottom = 1L, right = 1L)
        bmbl <- bm_shadow(x, value = value, bg = bg, extend = extend, bottom = 1L, left = 1L)
        bmtl <- bm_shadow(x, value = value, bg = bg, extend = extend, top = 1L, left = 1L)
        if (extend) {
            bmtr <- bm_extend(bmtr, value = bg, bottom = 1L, left = 1L)
            bmbr <- bm_extend(bmbr, value = bg, top = 1L, left = 1L)
            bmbl <- bm_extend(bmbl, value = bg, top = 1L, right = 1L)
            bmtl <- bm_extend(bmtl, value = bg, bottom = 1L, right = 1L)

        }
        bm_new <- bm_overlay(bm_new, under = bmtr, bg = bg)
        bm_new <- bm_overlay(bm_new, under = bmbr, bg = bg)
        bm_new <- bm_overlay(bm_new, under = bmbl, bg = bg)
        bm_new <- bm_overlay(bm_new, under = bmtl, bg = bg)
    }
    bm_new
}
