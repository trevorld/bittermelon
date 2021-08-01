#' Extend bitmaps
#'
#' `bm_extend()` extends [bm_bitmap()] objects with extra pixels.
#' The directions and the integer value of the extra pixels are settable
#' (defaulting to `0L`).
#'
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @param value Integer value for the new pixels.
#' @param sides If not `NULL` then an integer vector indicating how
#'              many pixels to pad on all four sides.
#'              If the integer vector is of length one it indicates the number of pixels for all four sides.
#'              If of length two gives first the number for the vertical sides and then the horizontal sides.
#'              If of length three gives the number of pixels for top, the horizontal sides, and then bottom sides.
#'              If of length four gives the number of pixels for top, right, bottom, and then left sides.
#'              This is the same scheme as used by the CSS padding and margin properties.
#' @param top How many pixels to pad the top.
#' @param right How many pixels to pad the right.
#' @param bottom How many pixels to pad the bottom.
#' @param left How many pixels to pad the left.
#' @param width How many pixels wide should the new bitmap be.
#'              Use with the `hjust` argument.
#' @param height How many pixels tall should the new bitmap be.
#'              Use with the `vjust` argument.
#' @param hjust One of "left", "center-left", "center-right", "right".
#'              "center-left" and "center-right" will attempt to
#'              place in "center" if possible but if not possible will bias
#'              it one pixel left or right respectively.
#'              "centre", "center", and "centre-left" are aliases for "center-left".
#'              "centre-right" is an alias for "center-right".
#' @param vjust One of "bottom", "center-bottom", "center-top", "top".
#'              "center-down" and "center-up" will attempt to
#'              place in "center" if possible but if not possible will bias
#'              it one pixel down or up respectively.
#'              "centre", "center", and "centre-up" are aliases for "center-up".
#'              "centre-down" is an alias for "center-down".
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  # add a border to an "R"
#'  capital_r <- font[[str2ucp("R")]]
#'  capital_r <- bm_extend(capital_r, value = 2L, sides = 1L)
#'  capital_r <- bm_extend(capital_r, value = 3L, sides = 1L)
#'  print(capital_r, labels = c(" ", "#", ".", "@"))
#' @export
bm_extend <- function(bm_object, value = 0L, sides = NULL,
                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                   width = NULL, height = NULL,
                   hjust = "center-left", vjust = "center-up") {
    modify_bm_bitmaps(bm_object, bm_extend_bitmap,
                      sides = sides, value = value,
                      top = top, right = right, bottom = bottom, left = left,
                      width = width, height = height,
                      hjust = hjust, vjust = vjust)
}

bm_extend_bitmap <- function(bitmap, value = 0L, sides = NULL,
                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                   width = NULL, height = NULL,
                   hjust = "center-left", vjust = "center-up") {
    stopifnot(is.null(sides) || is.null(top))
    stopifnot(is.null(sides) || is.null(right))
    stopifnot(is.null(sides) || is.null(bottom))
    stopifnot(is.null(sides) || is.null(left))
    stopifnot(is.null(sides) || is.null(width))
    stopifnot(is.null(sides) || is.null(height))
    stopifnot(is.null(height) || is.null(top))
    stopifnot(is.null(height) || is.null(bottom))
    stopifnot(is.null(width) || is.null(left))
    stopifnot(is.null(width) || is.null(right))

    d <- list(top = top %||% 0L, right = right %||% 0L,
              bottom = bottom %||% 0L, left = left %||% 0L)

    if (!is.null(sides))
        d <- adjust_d_sides(sides, d)
    if (!is.null(width))
        d <- adjust_d_width(bitmap, width, hjust, d)
    if (!is.null(height))
        d <- adjust_d_height(bitmap, height, vjust, d)

    bitmap <- bm_extend_top(bitmap, d$top, value)
    bitmap <- bm_extend_right(bitmap, d$right, value)
    bitmap <- bm_extend_bottom(bitmap, d$bottom, value)
    bitmap <- bm_extend_left(bitmap, d$left, value)
    bitmap
}

adjust_d_sides <- function(sides, d) {
    stopifnot(length(sides) < 5L)
    if (length(sides) == 1L) {
        d$top <- d$right <- d$bottom <- d$left <- sides
    } else if (length(sides) == 2L) {
        d$top <- d$bottom <- sides[1L]
        d$left <- d$right <- sides[2L]
    } else if (length(sides) == 3L) {
        d$top <- sides[1L]
        d$left <- d$right <- sides[2L]
        d$bottom <- sides[3L]
    } else {
        d$top <- sides[1L]
        d$right <- sides[2L]
        d$bottom <- sides[3L]
        d$left <- sides[4L]
    }
    d
}

adjust_d_width <- function(bitmap, width, hjust, d) {
    stopifnot(ncol(bitmap) <= width)
    if (hjust %in% c("center", "centre", "centre-left"))
        hjust <- "center-left"
    if (hjust == "centre-right")
        hjust <- "center-right"
    stopifnot(hjust %in% c("left", "center-left", "center-right", "right"))
    remainder <- width - ncol(bitmap)
    if (hjust == "left") {
        d$right <- remainder
    } else if (hjust == "right") {
        d$left <- remainder
    } else if (hjust == "center-left") {
        d$left <- remainder %/% 2
        d$right <- remainder - d$left
    } else { # center-right
        d$right <- remainder %/% 2
        d$left <- remainder - d$right
    }
    d
}

adjust_d_height <- function(bitmap, height, vjust, d) {
    stopifnot(nrow(bitmap) <= height)
    if (vjust %in% c("center", "centre", "centre-up"))
        vjust <- "center-up"
    if (vjust == "centre-down")
        vjust <- "center-down"
    stopifnot(vjust %in% c("top", "center-up", "center-down", "bottom"))
    remainder <- height - ncol(bitmap)
    if (vjust == "top") {
        d$bottom <- remainder
    } else if (vjust == "bottom") {
        d$top <- remainder
    } else if (vjust == "center-up") {
        d$top <- remainder %/% 2
        d$bottom <- remainder - d$top
    } else { # center-down
        d$bottom <- remainder %/% 2
        d$top <- remainder - d$bottom
    }
    d
}

bm_extend_top <- function(bitmap, n = 1L, value = 0L) {
    if (n == 0L) return(bitmap)
    new <- matrix(value, nrow = n, ncol = ncol(bitmap))
    rbind.bm_bitmap(new, bitmap)
}
bm_extend_right <- function(bitmap, n = 1L, value = 0L) {
    if (n == 0L) return(bitmap)
    new <- matrix(value, nrow = nrow(bitmap), ncol = n)
    cbind.bm_bitmap(bitmap, new)
}
bm_extend_bottom <- function(bitmap, n = 1L, value = 0L) {
    if (n == 0L) return(bitmap)
    new <- matrix(value, nrow = n, ncol = ncol(bitmap))
    rbind.bm_bitmap(bitmap, new)
}
bm_extend_left <- function(bitmap, n = 1L, value = 0L) {
    if (n == 0L) return(bitmap)
    new <- matrix(value, nrow = nrow(bitmap), ncol = n)
    cbind.bm_bitmap(new, bitmap)
}
