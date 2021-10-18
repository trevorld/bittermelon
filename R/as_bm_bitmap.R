#' Coerce to bitmap glyph objects
#'
#' `as_bm_bitmap()` turns an existing object into a `bm_bitmap()` object.
#'
#' @param x An object that can reasonably be coerced to a `bm_bitmap()` object.
#' @param ... Further arguments passed to or from other methods.
#' @return A `bm_bitmap()` object.
#' @examples
#'  space_matrix <- matrix(0L, nrow = 16L, ncol = 16L)
#'  space_glyph <- as_bm_bitmap(space_matrix)
#'  is_bm_bitmap(space_glyph)
#' @seealso [bm_bitmap()]
#' @export
as_bm_bitmap <- function(x, ...) {
    UseMethod("as_bm_bitmap")
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.matrix <- function(x, ...) {
    if (!is.integer(x)) {
        x[, ] <- suppressWarnings(as.integer(x))
    }
    stopifnot(!any(is.na(x)))
    class(x) <- c("bm_bitmap", class(x))
    x
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.default <- function(x, ...) {
    as_bm_bitmap.matrix(as.matrix(x))
}

#' @inheritParams as_bm_list
#' @inheritParams cbind.bm_bitmap
#' @inheritParams rbind.bm_bitmap
#' @rdname as_bm_bitmap
#' @param direction For purely horizontal binding either "left-to-right" (default) or its aliases "ltr" and "lr"
#'                  OR "right-to-left" or its aliases "rtl" and "rl".
#'                  For purley vertical binding either "top-to-bottom" (default) or its aliases "ttb" and "tb"
#'                  OR "bottom-to-top" or its aliases "btt" and "bt".
#'                  For character vectors of length greater than one: for first horizontal binding within values in the vector
#'                  and then vertical binding across values in the vector "left-to-right, top-to-bottom" (default)
#'                  or its aliases "lrtb" and "lr-tb"; "left-to-right, bottom-to-top" or its aliases "lrbt" and "lr-bt";
#'                  "right-to-left, top-to-bottom" or its aliases "rltb" and "rl-tb"; or
#'                  "right-to-left, bottom-to-top" or its aliases "rlbt" and "rl-bt".
#'                  For first vertical binding within values in the vector and then horizontal binding across values
#'                  "top-to-bottom, left-to-right" or its aliases "tblr" and "tb-lr";
#'                  "top-to-bottom, right-to-left" or its aliases "tbrl" and "tb-rl";
#'                  "bottom-to-top, left-to-right" or its aliases "btlr" and "bt-lr"; or
#'                  "bottom-to-top, right-to-left" or its aliases "btrl" and "bt-rl".
#'                  The `direction` argument is not case-sensitive.
#' @param compose Compose graphemes using [bm_compose()].
#' @param pua_combining Passed to [bm_compose()].
#' @examples
#'   font_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
#'   font <- read_yaff(font_file)
#'   bm <- as_bm_bitmap("RSTATS", font = font)
#'   print(bm, px = px_ascii)
#'   bm <- as_bm_bitmap("RSTATS", direction = "top-to-bottom", font = font)
#'   print(bm, px = px_ascii)
#' @export
as_bm_bitmap.character <- function(x, ...,
                                   direction = "left-to-right, top-to-bottom",
                                   font = bm_font(),
                                   hjust = "left",
                                   vjust = "top",
                                   compose = TRUE,
                                   pua_combining = character(0)) {

    direction <- tolower(direction)

    is_ltr <- direction %in% c("left-to-right", "ltr", "lr")
    is_rtl <- direction %in% c("right-to-left", "rtl", "rl")
    is_ttb <- direction %in% c("top-to-bottom", "ttb", "tb")
    is_btt <- direction %in% c("bottom-to-top", "btt", "bt")
    is_lrtb <- direction %in% c("left-to-right, top-to-bottom", "lrtb", "lr-tb")
    is_lrbt <- direction %in% c("left-to-right, bottom-to-top", "lrbt", "lr-bt")
    is_rltb <- direction %in% c("right-to-left, top-to-bottom", "rltb", "rl-tb")
    is_rlbt <- direction %in% c("right-to-left, bottom-to-top", "rlbt", "rl-bt")
    is_tblr <- direction %in% c("top-to-bottom, left-to-right", "tblr", "tb-lr")
    is_tbrl <- direction %in% c("top-to-bottom, right-to-left", "tbrl", "tb-rl")
    is_btlr <- direction %in% c("bottom-to-top, left-to-right", "btlr", "bt-lr")
    is_btrl <- direction %in% c("bottom-to-top, right-to-left", "btrl", "bt-rl")
    stopifnot(is_ltr || is_rtl || is_ttb || is_btt ||
              is_lrtb || is_lrbt || is_rltb || is_rlbt ||
              is_tblr || is_btlr || is_tbrl || is_btrl)
    if (is_ltr || is_rtl) {
        bml <- as_bm_list(x, font = font)
        if (compose) bml <- bm_compose(bml, pua_combining)
        bm <- bm_call(bml, cbind, direction = direction, vjust = vjust)
    } else if (is_ttb || is_btt) {
        bml <- as_bm_list(x, font = font)
        if (compose) bml <- bm_compose(bml, pua_combining)
        bm <- bm_call(bml, rbind, direction = direction, hjust = hjust)
    } else if (is_lrtb || is_lrbt || is_rltb || is_rlbt) {
        hdirection <- ifelse(is_lrtb || is_lrbt, "left-to-right", "right-to-left")
        vdirection <- ifelse(is_lrtb || is_rltb, "top-to-bottom", "bottom-to-top")
        l <- lapply(x, as_bm_list, font = font)
        if (compose) l <- lapply(l, bm_compose, pua_combining)
        l <- lapply(l, add_space, font = font)
        l <- lapply(l, function(x) bm_call(x, cbind, direction = hdirection, vjust = vjust))
        bm <- bm_call(l, rbind, direction = vdirection, hjust = hjust)
    } else {
        hdirection <- ifelse(is_tblr || is_btlr, "left-to-right", "right-to-left")
        vdirection <- ifelse(is_tblr || is_tbrl, "top-to-bottom", "bottom-to-top")
        l <- lapply(x, as_bm_list, font = font)
        if (compose) l <- lapply(l, bm_compose, pua_combining)
        l <- lapply(l, add_space, font = font)
        l <- lapply(l, function(x) bm_call(x, rbind, direction = vdirection, hjust = hjust))
        bm <- bm_call(l, cbind, direction = hdirection, vjust = vjust)
    }
    bm
}

# If a line of text is empty fill it with "space" instead
add_space <- function(bml, font) {
    if (length(bml) > 0) {
        bml
    } else {
        font["U+0020"]
    }
}

#' @rdname as_bm_bitmap
#' @param width Desired width of bitmap
#' @param height Desired height of bitmap
#' @param png_device A function taking arguments `filename`, `width`, and `height`
#'                   that starts a graphics device that saves a png image
#'                   with a transparent background.  By default will use [ragg::agg_png()]
#'                   if available else the \dQuote{cairo} version of [grDevices::png()]
#'                   if available else just [grDevices::png()].
#' @param threshold  If any png channel weakly exceeds this threshold
#'                   (on an interval from zero to one)
#'                   then the pixel is determined to be \dQuote{black}.
#' @examples
#'
#'  if (require("grid") && capabilities("png")) {
#'    circle <- as_bm_bitmap(circleGrob(r = 0.25), width = 16L, height = 16L)
#'    print(circle, px = c(".", "@"))
#'
#'    inverted_exclamation <- as_bm_bitmap(textGrob("!", rot = 180),
#'                                         width = 8L, height = 16L)
#'    print(inverted_exclamation, px = c(".", "@"))
#'  }
#' @importFrom grid gpar grob grid.draw pushViewport popViewport viewport
#' @export
as_bm_bitmap.grob <- function(x, ..., width = 8L, height = 16L,
                              png_device = NULL, threshold = 0.25) {
    current_dev <- grDevices::dev.cur()
    if (current_dev > 1L)
        on.exit(grDevices::dev.set(current_dev))

    png_file <- tempfile(fileext = ".png")
    on.exit(unlink(png_file))

    if (is.null(png_device))
        png_device <- default_png_device()

    png_device(filename = png_file, height = height, width = width)
    pushViewport(viewport(gp = gpar(lwd = 0, col = "black", fill = "black")))
    grid.draw(x)
    popViewport()
    grDevices::dev.off()

    array_glyph <- png::readPNG(png_file, native = FALSE)
    m_bitmap <- apply(array_glyph, c(1, 2), function(x) as.integer(any(x >= threshold)))
    bm_bitmap(m_bitmap[rev(seq.int(nrow(m_bitmap))), ])
}

default_png_device <- function() {
    if (requireNamespace("ragg", quietly = TRUE)) {
        function(filename, width, height)
            ragg::agg_png(filename, width, height, background = "transparent")
    } else if (capabilities("png") && capabilities("cairo")) {
        function(filename, width, height)
            grDevices::png(filename, width, height, bg = "transparent", type = "cairo")
    } else {
        stopifnot(capabilities("png"))
        function(filename, width, height)
            grDevices::png(filename, width, height, bg = "transparent")
    }
}
