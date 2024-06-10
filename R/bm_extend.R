#' Extend bitmaps on the sides with extra pixels
#'
#' `bm_extend()` extends [bm_bitmap()] objects with extra pixels.
#' The directions and the integer value of the extra pixels are settable
#' (defaulting to `0L`).
#'
#' @inheritParams bm_clamp
#' @param value Value for the new pixels.
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
#'              Use with the `hjust` argument or just one of either the `left` or `right` arguments.
#' @param height How many pixels tall should the new bitmap be.
#'              Use with the `vjust` argument or just one of either the `top` or `bottom` arguments.
#' @param hjust One of "left", "center-left", "center-right", "right".
#'              "center-left" and "center-right" will attempt to
#'              place in "center" if possible but if not possible will bias
#'              it one pixel left or right respectively.
#'              "centre", "center", and "centre-left" are aliases for "center-left".
#'              "centre-right" is an alias for "center-right".
#' @param vjust One of "bottom", "center-bottom", "center-top", "top".
#'              "center-bottom" and "center-top" will attempt to
#'              place in "center" if possible but if not possible will bias
#'              it one pixel down or up respectively.
#'              "centre", "center", and "centre-top" are aliases for "center-top".
#'              "centre-bottom" is an alias for "center-bottom".
#' @inherit bm_clamp return
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' # add a border to an "R"
#' capital_r <- font[[str2ucp("R")]]
#' capital_r <- bm_extend(capital_r, value = 2L, sides = 1L)
#' capital_r <- bm_extend(capital_r, value = 3L, sides = 1L)
#' print(capital_r)
#'
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' corn_framed <- bm_extend(corn, value = "brown", sides = 1L)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_framed, compress = "v")
#' }
#' @seealso [bm_expand()], [bm_pad()], [bm_resize()], and [bm_trim()].
#' @export
bm_extend <- function(x, value,  sides = NULL,
                      top = NULL, right = NULL, bottom = NULL, left = NULL,
                      width = NULL, height = NULL,
                      hjust = "center-left", vjust = "center-top") {
    stopifnot(missing(sides) || missing(top))
    stopifnot(missing(sides) || missing(right))
    stopifnot(missing(sides) || missing(bottom))
    stopifnot(missing(sides) || missing(left))
    stopifnot(missing(sides) || missing(width))
    stopifnot(missing(sides) || missing(height))
    stopifnot(missing(height) || (missing(top)) || missing(bottom))
    stopifnot(missing(width) || (missing(left)) || missing(right))
    stopifnot(missing(hjust) || (missing(left) && missing(right)))
    stopifnot(missing(vjust) || (missing(left) && missing(right)))
    UseMethod("bm_extend")
}

#' @rdname bm_extend
#' @export
bm_extend.bm_bitmap <- function(x, value = 0L, sides = NULL,
                                top = NULL, right = NULL, bottom = NULL, left = NULL,
                                width = NULL, height = NULL,
                                hjust = "center-left", vjust = "center-top") {
    bm_extend_bitmap(x, value = value,
                     sides = sides,
                     top = top, right = right, bottom = bottom, left = left,
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

#' @rdname bm_extend
#' @export
bm_extend.bm_pixmap <- function(x, value = col2hex("transparent"), sides = NULL,
                                top = NULL, right = NULL, bottom = NULL, left = NULL,
                                width = NULL, height = NULL,
                                hjust = "center-left", vjust = "center-top") {
    value <- col2hex(value)
    bm_extend_bitmap(x, value = value,
                     sides = sides,
                     top = top, right = right, bottom = bottom, left = left,
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

# #' @rdname bm_extend
# #' @export
# bm_extend.bm_list <- function(x, value = 0L, sides = NULL,
#                               top = NULL, right = NULL, bottom = NULL, left = NULL,
#                               width = NULL, height = NULL,
#                               hjust = "center-left", vjust = "center-top") {
#     bm_lapply(x, bm_extend_bitmap, value = value,
#               sides = sides,
#               top = top, right = right, bottom = bottom, left = left,
#               width = width, height = height,
#               hjust = hjust, vjust = vjust)
# }

#' @rdname bm_extend
#' @export
bm_extend.bm_list <- function(x, ...) {
    bm_lapply(x, bm_extend, ...)
}

# A future `magick::image_splice()` may be used for simple extensions.

#' @rdname bm_extend
#' @export
`bm_extend.magick-image` <- function(x, value = "transparent", sides = NULL,
                                     top = NULL, right = NULL, bottom = NULL, left = NULL,
                                     width = NULL, height = NULL,
                                     hjust = "center-left", vjust = "center-top") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    pm <- `as_bm_pixmap.magick-image`(x)
    value <- col2hex(value)
    pm <- bm_extend_bitmap(pm, value = value,
                           sides = sides,
                           top = top, right = right, bottom = bottom, left = left,
                           width = width, height = height,
                           hjust = hjust, vjust = vjust)
    magick::image_read(pm)
}

#' @rdname bm_extend
#' @export
bm_extend.nativeRaster <- function(x, value = col2int("transparent"), sides = NULL,
                              top = NULL, right = NULL, bottom = NULL, left = NULL,
                              width = NULL, height = NULL,
                              hjust = "center-left", vjust = "center-top") {
    value <- int2col(as_native(value))
    pm <- bm_extend_bitmap(as_bm_pixmap.nativeRaster(x),
                           value = value, sides = sides,
                           top = top, right = right, bottom = bottom, left = left,
                           width = width, height = height,
                           hjust = hjust, vjust = vjust)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_extend
#' @export
bm_extend.raster <- function(x, value = "transparent", sides = NULL,
                              top = NULL, right = NULL, bottom = NULL, left = NULL,
                              width = NULL, height = NULL,
                              hjust = "center-left", vjust = "center-top") {
    bm_extend_bitmap(x, value = value,
                     sides = sides,
                     top = top, right = right, bottom = bottom, left = left,
                     width = width, height = height,
                     hjust = hjust, vjust = vjust)
}

bm_extend_bitmap <- function(x, value = 0L, sides = NULL,
                             top = NULL, right = NULL, bottom = NULL, left = NULL,
                             width = NULL, height = NULL,
                             hjust = "center-left", vjust = "center-top") {

    d <- list(top = top %||% 0L, right = right %||% 0L,
              bottom = bottom %||% 0L, left = left %||% 0L)

    if (!is.null(sides))
        d <- adjust_d_sides(sides, d)
    if (!is.null(width))
        d <- adjust_d_width_extend(x, width, hjust, d, left, right)
    if (!is.null(height))
        d <- adjust_d_height_extend(x, height, vjust, d, top, bottom)
    stopifnot(min(unlist(d)) >= 0L)

    if (d$top > 0L)
        x <- bm_extend_helper(x, d$top, value, "top")
    if (d$right > 0L)
        x <- bm_extend_helper(x, d$right, value, "right")
    if (d$bottom > 0L)
        x <- bm_extend_helper(x, d$bottom, value, "bottom")
    if (d$left > 0L)
        x <- bm_extend_helper(x, d$left, value, "left")
    x
}

bm_extend_helper <- function(x, n, value, direction) {
    UseMethod("bm_extend_helper")
}

#' @export
bm_extend_helper.bm_bitmap <- function(x, n = 1L, value = 0L, direction = "top") {
    switch(direction,
           top = {
               new <- as_bm_bitmap.matrix(matrix(value, nrow = n, ncol = ncol(x)))
               rbind.bm_bitmap(new, x)
           },
           right = {
               new <- as_bm_bitmap.matrix(matrix(value, nrow = nrow(x), ncol = n))
               cbind.bm_bitmap(x, new)
           },
           bottom = {
               new <- as_bm_bitmap.matrix(matrix(value, nrow = n, ncol = ncol(x)))
               rbind.bm_bitmap(x, new)
           },
           left = {
               new <- as_bm_bitmap.matrix(matrix(value, nrow = nrow(x), ncol = n))
               cbind.bm_bitmap(new, x)
           })
}

#' @export
bm_extend_helper.bm_pixmap <- function(x, n = 1L, value = 0L, direction = "top") {
    switch(direction,
           top = {
               new <- as_bm_pixmap.matrix(matrix(value, nrow = n, ncol = ncol(x)))
               rbind.bm_pixmap(new, x)
           },
           right = {
               new <- as_bm_pixmap.matrix(matrix(value, nrow = nrow(x), ncol = n))
               cbind.bm_pixmap(x, new)
           },
           bottom = {
               new <- as_bm_pixmap.matrix(matrix(value, nrow = n, ncol = ncol(x)))
               rbind.bm_pixmap(x, new)
           },
           left = {
               new <- as_bm_pixmap.matrix(matrix(value, nrow = nrow(x), ncol = n))
               cbind.bm_pixmap(new, x)
           })
}

#' @export
bm_extend_helper.raster <- function(x, n = 1L, value = "transparent", direction = "top") {
    x <- as.matrix(x)
    x <- switch(direction,
           top = {
               new <- matrix(value, nrow = n, ncol = ncol(x))
               rbind(new, x)
           },
           right = {
               new <- matrix(value, nrow = nrow(x), ncol = n)
               cbind(x, new)
           },
           bottom = {
               new <- matrix(value, nrow = n, ncol = ncol(x))
               rbind(x, new)
           },
           left = {
               new <- matrix(value, nrow = nrow(x), ncol = n)
               cbind(new, x)
           })
    as.raster(x)
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

adjust_d_width_extend <- function(x, width, hjust, d, left, right) {
    stopifnot(ncol(x) <= width)
    remainder <- width - ncol(x)
    adjust_d_width(remainder, width, hjust, d, left, right)
}

adjust_d_width <- function(remainder, width, hjust, d, left, right) {
    if (hjust %in% c("center", "centre", "centre-left"))
        hjust <- "center-left"
    if (hjust == "centre-right")
        hjust <- "center-right"
    stopifnot(hjust %in% c("left", "center-left", "center-right", "right"))
    if (is.null(left) && is.null(right)) {
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
    } else if (is.null(left)) {
        d$right <- right
        d$left <- remainder - d$right
    } else { # is.null(right) is true
        d$left <- left
        d$right <- remainder - d$left
    }
    d
}

adjust_d_height_extend <- function(x, height, vjust, d, top, bottom) {
    stopifnot(nrow(x) <= height)
    remainder <- height - nrow(x)
    adjust_d_height(remainder, height, vjust, d, top, bottom)
}

adjust_d_height <- function(remainder, height, vjust, d, top, bottom) {
    if (vjust %in% c("center", "centre", "centre-top"))
        vjust <- "center-top"
    if (vjust %in% c("centre-bottom"))
        vjust <- "center-bottom"
    stopifnot(vjust %in% c("top", "center-top", "center-bottom", "bottom"))
    if (is.null(top) && is.null(bottom)) {
        if (vjust == "top") {
            d$bottom <- remainder
        } else if (vjust == "bottom") {
            d$top <- remainder
        } else if (vjust == "center-top") {
            d$top <- remainder %/% 2
            d$bottom <- remainder - d$top
        } else { # center-bottom
            d$bottom <- remainder %/% 2
            d$top <- remainder - d$bottom
        }
    } else if (is.null(top)) {
        d$bottom <- bottom
        d$top <- remainder - d$bottom
    } else { # is.null(bottom) is true
        d$top <- top
        d$bottom <- remainder - d$top
    }
    d
}
