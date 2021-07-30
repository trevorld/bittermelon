#' Extend bitmaps
#'
#' `bm_extend()` extends [bm_bitmap()] objects with extra pixels.
#' The directions and the integer value of the extra pixels are settable
#' (defaulting to `0L`).
#'
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @param value Integer value for the new pixels.
#' @param padding If not `NULL` then an integer vector indicating how
#'                many pixels to pad each side.
#'                If length one indicates the number for all sides.
#'                If length two the number for the vertical and then horizontal sides.
#'                If length three the number for top, horizontal, and bottom sides.
#'                If length four the number for top, right, bottom, and left.
#'                This is the same scheme as used by the CSS padding property.
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
#'  capital_r <- bm_extend(capital_r, value = 2L, padding = 1L)
#'  capital_r <- bm_extend(capital_r, value = 3L, padding = 1L)
#'  print(capital_r, labels = c(" ", "#", ".", "@"))
#' @export
bm_extend <- function(bm_object, value = 0L, padding = NULL,
                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                   width = NULL, height = NULL,
                   hjust = "center-left", vjust = "center-up") {
    modify_bm_bitmaps(bm_object, bm_extend_bitmap,
                      padding = padding, value = value,
                      top = top, right = right, bottom = bottom, left = left,
                      width = width, height = height,
                      hjust = hjust, vjust = vjust)
}

bm_extend_bitmap <- function(bitmap, value = 0L, padding = NULL,
                   top = NULL, right = NULL, bottom = NULL, left = NULL,
                   width = NULL, height = NULL,
                   hjust = "center-left", vjust = "center-up") {
    stopifnot(is.null(padding) || is.null(top))
    stopifnot(is.null(padding) || is.null(right))
    stopifnot(is.null(padding) || is.null(bottom))
    stopifnot(is.null(padding) || is.null(left))
    stopifnot(is.null(padding) || is.null(width))
    stopifnot(is.null(padding) || is.null(height))
    stopifnot(is.null(height) || is.null(top))
    stopifnot(is.null(height) || is.null(bottom))
    stopifnot(is.null(width) || is.null(left))
    stopifnot(is.null(width) || is.null(right))

    d <- list(top = top %||% 0L, right = right %||% 0L,
              bottom = bottom %||% 0L, left = left %||% 0L)

    if (!is.null(padding))
        d <- adjust_d_padding(padding, d)
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

adjust_d_padding <- function(padding, d) {
    stopifnot(length(padding) < 5L)
    if (length(padding) == 1L) {
        d$top <- d$right <- d$bottom <- d$left <- padding
    } else if (length(padding) == 2L) {
        d$top <- d$bottom <- padding[1L]
        d$left <- d$right <- padding[2L]
    } else if (length(padding) == 3L) {
        d$top <- padding[1L]
        d$left <- d$right <- padding[2L]
        d$bottom <- padding[3L]
    } else {
        d$top <- padding[1L]
        d$right <- padding[2L]
        d$bottom <- padding[3L]
        d$left <- padding[4L]
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
