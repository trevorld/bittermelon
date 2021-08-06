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
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = px_ascii)
#'  print(bm_shadow(capital_r), px = px_ascii)
#'  print(bm_bold(capital_r), px = px_ascii)
#'  print(bm_glow(capital_r), px = px_ascii)
#'  print(bm_glow(capital_r, corner = TRUE), px = px_ascii)
#'
#' @inherit bm_clamp return
#' @seealso [bm_extend()] and [bm_shift()]
#' @export
bm_shadow <- function(bm_object, value = 2L,
                      top = NULL, right = NULL, bottom = NULL, left = NULL,
                      extend = TRUE) {
    stopifnot(is.null(top) || is.null(bottom))
    stopifnot(is.null(left) || is.null(right))
    if (is.null(top) && is.null(right) && is.null(bottom) && is.null(left)) {
        right <- 1L
        bottom <- 1L
    }
    if (extend)
        bm_object <- bm_extend(bm_object, top = top, right = right, bottom = bottom, left = left)
    modify_bm_bitmaps(bm_object, bm_shadow_bitmap, value = value,
                      top = top, right = right, bottom = bottom, left = left)
}


bm_shadow_bitmap <- function(bitmap, value = 2L,
                      top = NULL, right = NULL, bottom = NULL, left = NULL) {
    shadow <- bm_shift(bitmap, top = top, right = right, bottom = bottom, left = left)
    shadow[which(shadow > 0L)] <- as.integer(value)
    bm_overlay(shadow, bitmap)
}

#' @rdname bm_shadow
#' @export
bm_bold <- function(bm_object, value = 1L,
                    top = NULL, right = NULL, bottom = NULL, left = NULL,
                    extend = TRUE) {
    stopifnot(is.null(top) || is.null(bottom))
    stopifnot(is.null(left) || is.null(right))
    if (is.null(top) && is.null(right) && is.null(bottom) && is.null(left)) {
        right <- 1L
    }
    bm_shadow(bm_object, value = value,
              top = top, right = right, bottom = bottom, left = left, extend = extend)
}

#' @rdname bm_shadow
#' @export
bm_glow <- function(bm_object, value = 2L, extend = TRUE, corner = FALSE) {
    modify_bm_bitmaps(bm_object, bm_glow_bitmap, value = value,
                      extend = extend, corner = corner)
}

bm_glow_bitmap <- function(bm_object, value = 2L, extend = TRUE, corner = FALSE) {
    bmt <- bm_shadow(bm_object, value = value, extend = extend, top = 1L)
    bmr <- bm_shadow(bm_object, value = value, extend = extend, right = 1L)
    bmb <- bm_shadow(bm_object, value = value, extend = extend, bottom = 1L)
    bml <- bm_shadow(bm_object, value = value, extend = extend, left = 1L)
    if (extend) {
        bm_new <- bm_extend(bm_object, sides = 1L)
        bmt <- bm_extend(bmt, right = 1L, bottom = 1L, left = 1L)
        bmr <- bm_extend(bmr, top = 1L, bottom = 1L, left = 1L)
        bmb <- bm_extend(bmb, top = 1L, right = 1L, left = 1L)
        bml <- bm_extend(bml, top = 1L, right = 1L, bottom = 1L)
    } else {
        bm_new <- bm_object
    }
    bm_new <- bm_overlay(bm_new, under = bmt)
    bm_new <- bm_overlay(bm_new, under = bmr)
    bm_new <- bm_overlay(bm_new, under = bmb)
    bm_new <- bm_overlay(bm_new, under = bml)
    if (corner) {
        bmtr <- bm_shadow(bm_object, value = value, extend = extend, top = 1L, right = 1L)
        bmbr <- bm_shadow(bm_object, value = value, extend = extend, bottom = 1L, right = 1L)
        bmbl <- bm_shadow(bm_object, value = value, extend = extend, bottom = 1L, left = 1L)
        bmtl <- bm_shadow(bm_object, value = value, extend = extend, top = 1L, left = 1L)
        if (extend) {
            bmtr <- bm_extend(bmtr, bottom = 1L, left = 1L)
            bmbr <- bm_extend(bmbr, top = 1L, left = 1L)
            bmbl <- bm_extend(bmbl, top = 1L, right = 1L)
            bmtl <- bm_extend(bmtl, bottom = 1L, right = 1L)

        }
        bm_new <- bm_overlay(bm_new, under = bmtr)
        bm_new <- bm_overlay(bm_new, under = bmbr)
        bm_new <- bm_overlay(bm_new, under = bmbl)
        bm_new <- bm_overlay(bm_new, under = bmtl)
    }
    bm_new
}
