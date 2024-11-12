#' Cast to a bitmap matrix object
#'
#' `as_bm_bitmap()` turns an existing object into a `bm_bitmap()` object.
#'
#' @param x An object that can reasonably be coerced to a `bm_bitmap()` object.
#' @param ... Further arguments passed to or from other methods.
#' @return A `bm_bitmap()` object.
#' @seealso [bm_bitmap()]
#' @export
as_bm_bitmap <- function(x, ...) {
    UseMethod("as_bm_bitmap")
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.array <- function(x, ...,
                               mode = c("alpha", "darkness", "brightness"),
                               threshold = 0.5) {
    # If no alpha channel then default to `mode <- "darkness"`
    # One channel won't be dispatched by this so just worry about 3-channels
    if (missing(mode) && dim(x)[3L] == 3L) { # RGB
        mode <- "darkness"
    } else {
        mode <- match.arg(mode)
    }
    if (dim(x)[3L] == 2L) { # GA
        grey <- as.double(x[, , 1L])
        alpha <- as.double(x[, , 2L])
        m <- matrix(grDevices::rgb(grey, grey, grey, alpha = alpha),
                    nrow = nrow(x), ncol = ncol(x))
        as_bm_bitmap.bm_pixmap(as_bm_pixmap.matrix(flip_matrix_vertically(m)),
                               mode = mode, threshold = threshold)
    } else { # RGB or RGBA
        as_bm_bitmap.bm_pixmap(as_bm_pixmap.array(x),
                               mode = mode, threshold = threshold)
    }
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.default <- function(x, ...) {
    # "bitmap" "rgba" array from `pdftools::pdf_render_page()`
    if (is.array(x) && is.raw(x))
        as_bm_bitmap.bm_pixmap(as_bm_pixmap.array(x), ...)
    else
        as_bm_bitmap.matrix(as.matrix(x))
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.bm_bitmap <- function(x, ...) {
    x
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.bm_pixmap <- function(x, ...,
                                   mode = c("alpha", "darkness", "brightness"),
                                   threshold = 0.5) {
    mode <- match.arg(mode)
    opaque <- switch(mode,
                     alpha = hex2alpha(as.character(x)),
                     darkness = hex2darkness(as.character(x)),
                     brightness = hex2brightness(as.character(x))) >= threshold
    m <- matrix(as.integer(opaque), nrow = nrow(x), ncol = ncol(x))
    class(m) <- c("bm_bitmap", "bm_matrix", class(m))
    m
}

#' @inheritParams as_bm_list
#' @inheritParams cbind.bm_bitmap
#' @inheritParams rbind.bm_bitmap
#' @rdname as_bm_bitmap
#' @param direction For purely horizontal binding either "left-to-right" (default) or its aliases "ltr" and "lr"
#'                  OR "right-to-left" or its aliases "rtl" and "rl".
#'                  For purley vertical binding either "top-to-bottom" (default) or its aliases "ttb" and "tb"
#'                  OR "bottom-to-top" or its aliases "btt" and "bt".
#'                  For character vectors of length greater than one: for first horizontal binding within
#'                  values in the vector
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
#' space_matrix <- matrix(0L, nrow = 16L, ncol = 16L)
#' space_glyph <- as_bm_bitmap(space_matrix)
#' is_bm_bitmap(space_glyph)
#'
#' font_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
#' font <- read_yaff(font_file)
#' bm <- as_bm_bitmap("RSTATS", font = font)
#' print(bm)
#'
#' bm <- as_bm_bitmap("RSTATS", direction = "top-to-bottom", font = font)
#' print(bm)
#'
#' if (require("grid") && capabilities("png")) {
#'   circle <- as_bm_bitmap(circleGrob(r = 0.25), width = 16L, height = 16L)
#'   print(circle)
#' }
#'
#' if (require("grid") && capabilities("png")) {
#'   inverted_exclamation <- as_bm_bitmap(textGrob("!", rot = 180),
#'                                        width = 8L, height = 16L)
#'   print(inverted_exclamation)
#' }
#'
#' if (requireNamespace("mazing", quietly = TRUE)) {
#'   m <- mazing::maze(16, 32)
#'   bm <- as_bm_bitmap(m, walls = TRUE)
#'   print(bm, compress = "vertical")
#' }
#'
#' if (requireNamespace("gridpattern", quietly = TRUE)) {
#'   w <- gridpattern::pattern_weave("twill_herringbone", nrow=14L, ncol = 40L)
#'   bm <- as_bm_bitmap(w)
#'   print(bm, compress = "vertical")
#' }
#' @export
as_bm_bitmap.character <- function(x, ...,
                                   direction = "left-to-right, top-to-bottom",
                                   font = bm_font(),
                                   hjust = "left",
                                   vjust = "top",
                                   compose = TRUE,
                                   pua_combining = character(0)) {

    direction <- tolower(direction)
    check_direction(direction)
    direction_type <- get_direction_type(direction)
    if (direction_type == "h") {
        bml <- as_bm_list(x, font = font)
        if (compose) bml <- bm_compose(bml, pua_combining)
        bm <- bm_call(bml, cbind, direction = direction, vjust = vjust)
    } else if (direction_type == "v") {
        bml <- as_bm_list(x, font = font)
        if (compose) bml <- bm_compose(bml, pua_combining)
        bm <- bm_call(bml, rbind, direction = direction, hjust = hjust)
    } else if (direction_type == "hv") {
        hdirection <- get_h_direction(direction)
        vdirection <- get_v_direction(direction)
        l <- lapply(x, as_bm_list, font = font)
        if (compose) l <- lapply(l, bm_compose, pua_combining)
        l <- lapply(l, add_space, font = font)
        l <- lapply(l, function(x) bm_call(x, cbind, direction = hdirection, vjust = vjust))
        bm <- bm_call(l, rbind, direction = vdirection, hjust = hjust)
    } else { # vh
        hdirection <- get_h_direction(direction)
        vdirection <- get_v_direction(direction)
        l <- lapply(x, as_bm_list, font = font)
        if (compose) l <- lapply(l, bm_compose, pua_combining)
        l <- lapply(l, add_space, font = font)
        l <- lapply(l, function(x) bm_call(x, rbind, direction = vdirection, hjust = hjust))
        bm <- bm_call(l, cbind, direction = hdirection, vjust = vjust)
    }
    bm
}

check_direction <- function(direction) {

    stopifnot(direction %in% c("left-to-right", "ltr", "lr",
                               "right-to-left", "rtl", "rl",
                               "top-to-bottom", "ttb", "tb",
                               "bottom-to-top", "btt", "bt",
                               "left-to-right, top-to-bottom", "lrtb", "lr-tb",
                               "left-to-right, bottom-to-top", "lrbt", "lr-bt",
                               "right-to-left, top-to-bottom", "rltb", "rl-tb",
                               "right-to-left, bottom-to-top", "rlbt", "rl-bt",
                               "top-to-bottom, left-to-right", "tblr", "tb-lr",
                               "top-to-bottom, right-to-left", "tbrl", "tb-rl",
                               "bottom-to-top, left-to-right", "btlr", "bt-lr",
                               "bottom-to-top, right-to-left", "btrl", "bt-rl"))
}

get_direction_type <- function(direction) {
    is_h <- direction %in% c("left-to-right", "ltr", "lr", "right-to-left", "rtl", "rl")
    is_v <- direction %in% c("top-to-bottom", "ttb", "tb", "bottom-to-top", "btt", "bt")
    is_hv <- direction %in% c("left-to-right, top-to-bottom", "lrtb", "lr-tb",
                              "left-to-right, bottom-to-top", "lrbt", "lr-bt",
                              "right-to-left, top-to-bottom", "rltb", "rl-tb",
                              "right-to-left, bottom-to-top", "rlbt", "rl-bt")

    if (is_h) {
        "h"
    } else if (is_v) {
       "v"
    } else if (is_hv) {
        "hv"
    } else {
        "vh"
    }
}

get_h_direction <- function(direction) {
    if (grepl("left-to-right|ltr|lr", direction))
        "left-to-right"
    else
        "right-to-left"
}

get_v_direction <- function(direction) {
    if (grepl("top-to-bottom|ttb|tb", direction))
        "top-to-bottom"
    else
        "bottom-to-top"
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
#' @export
as_bm_bitmap.glyph_bitmap <- function(x, ..., threshold = 0.5) {
    ints <- as.integer(as.integer(x) / 255 >= threshold)
    m <- matrix(ints, nrow = nrow(x), ncol = ncol(x))
    as_bm_bitmap.matrix(flip_matrix_vertically(m))
}

#' @rdname as_bm_bitmap
#' @param width Desired width of bitmap
#' @param height Desired height of bitmap
#' @param png_device A function taking arguments `filename`, `width`, and `height`
#'                   that starts a graphics device that saves a png image
#'                   with a transparent background.  By default will use [ragg::agg_png()]
#'                   if available else the \dQuote{cairo} version of [grDevices::png()]
#'                   if available else just [grDevices::png()].
#' @param mode Method to determine the integer values of the `bm_bitmap()` object:
#'             \describe{
#'             \item{alpha}{Higher alpha values get a `1L`.}
#'             \item{darkness}{Higher darkness values get a `1L`.  `darkness = (1 - luma) * alpha`.}
#'             \item{brightness}{Higher brightness values get a `1L`.  `brightness = luma * alpha`.}
#'             }
#' @param threshold  If the alpha/darkness/brightness value
#'                   weakly exceeds this threshold
#'                   (on an interval from zero to one)
#'                   then the pixel is determined to be \dQuote{black}.
#' @importFrom grid gpar grob grid.draw pushViewport popViewport viewport
#' @export
as_bm_bitmap.grob <- function(x, ..., width = 8L, height = 16L,
                              png_device = NULL, threshold = 0.25) {
    stopifnot(width > 1L, height > 1L) # guarantee `m_bitmap` is a matrix
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

    a <- png::readPNG(png_file, native = FALSE)
    as_bm_bitmap.array(a, threshold = threshold)
}

#' @rdname as_bm_bitmap
#' @export
`as_bm_bitmap.lofi-matrix` <- function(x, ...) {
    bm <- as_bm_bitmap.matrix(unclass(x))
    bm_flip(bm)
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

#' @rdname as_bm_bitmap
#' @export
`as_bm_bitmap.magick-image` <- function(x, ...,
                                        mode = c("alpha", "darkness", "brightness"),
                                        threshold = 0.5) {
    mode <- match.arg(mode)
    as_bm_bitmap.bm_pixmap(`as_bm_pixmap.magick-image`(x),
                           mode = mode, threshold = threshold)
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.matrix <- function(x, ...) {
    if (!is.integer(x)) {
        if (is.double(x))
            x[, ] <- suppressWarnings(as.integer(round(x)))
        else
            x[, ] <- suppressWarnings(as.integer(x))
    }
    stopifnot(!any(is.na(x)))
    class(x) <- c("bm_bitmap", "bm_matrix", class(x))
    x
}

#' @rdname as_bm_bitmap
#' @param walls If `TRUE` the values of 1L denote the walls and the values of 0L denote the paths.
#' @param start,end If not `NULL` mark the `start` and `end` as value 2L.
#'                  See [mazing::find_maze_refpoint()].
#' @param solve If `TRUE` then mark the solution path from `start` to `end` as value 3L.
#'              See [mazing::solve_maze()].
#' @export
as_bm_bitmap.maze <- function(x, ..., walls = FALSE, start = NULL, end = NULL,
                              solve = !is.null(start) && !is.null(end)) {
    stopifnot(requireNamespace("mazing", quietly = TRUE))
    b <- mazing::maze2binary(x)
    if (walls)
        b <- !b
    bm <- as_bm_bitmap.matrix(b)
    if (!is.null(start) && !is.null(end)) {
        if (solve) {
            path <- mazing::solve_maze(x, start = start, end = end)

            # Can get rid of this once `solve_maze()` supports `by = 0.5`
            path2 <- matrix(0, nrow = 2L * nrow(path) - 1L, ncol = 2L)
            path2[seq.int(1L, 2L * nrow(path) - 1L, 2L), 1L] <- path[, 1L]
            path2[seq.int(1L, 2L * nrow(path) - 1L, 2L), 2L] <- path[, 2L]
            for (i in seq_len(nrow(path) - 1)) {
                path2[2L * i, 1L] <- mean(path[c(i, i+1L), 1L])
                path2[2L * i, 2L] <- mean(path[c(i, i+1L), 2L])
            }
            path <- path2

            for (i in seq_len(nrow(path))) {
                bm[2L * path[i, 2L], 2L * path[i, 1L]] <- 3L
            }
        }
        yxs <- mazing::find_maze_refpoint(start, x)
        yxe <- mazing::find_maze_refpoint(end, x)
        bm[2 * yxs[2L], 2 * yxs[1L]] <- 2L
        bm[2 * yxe[2L], 2 * yxe[1L]] <- 2L
    }
    bm
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.nativeRaster <- function(x, ...,
                                      mode = c("alpha", "darkness", "brightness"),
                                      threshold = 0.5) {
    mode <- match.arg(mode)
    as_bm_bitmap.bm_pixmap(as_bm_pixmap.nativeRaster(x),
                           mode = mode, threshold = threshold)
}

# #' @rdname as_bm_bitmap
# #' @export
# as_bm_bitmap.pattern_hex <- function(x, ...) {
#     m <- matrix(as.integer(x), nrow = nrow(x), ncol = ncol(x))
#     as_bm_bitmap(m)
# }

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.pattern_square <- function(x, ...) {
    m <- matrix(as.integer(x) - 1L, nrow = nrow(x), ncol = ncol(x))
    as_bm_bitmap(m)
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.pattern_weave <- function(x, ...) {
    m <- matrix(as.integer(x), nrow = nrow(x), ncol = ncol(x))
    as_bm_bitmap(m)
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.pattern_square <- function(x, ...) {
    m <- x - 1L
    class(m) <- c("bm_bitmap", "bm_matrix", class(matrix()))
    m
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.pixeltrix <- function(x, ...) {
    m <- matrix(as.integer(x), nrow = nrow(x), ncol = ncol(x))
    as_bm_bitmap(flip_matrix_vertically(m))
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.pixmapGrey <- function(x, ...,
                                    mode = c("darkness", "brightness"),
                                    threshold = 0.5) {
    mode <- match.arg(mode)
    grey <- switch(mode,
                   brightness = as.double(x@grey),
                   darkness = 1 - as.double(x@grey)
                   )
    m <- flip_matrix_vertically(matrix(as.matrix(grey >= threshold),
                                       nrow = x@size[1L], ncol = x@size[2L]))
    as_bm_bitmap.matrix(m)
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.pixmapIndexed <- function(x, ...) {
    as_bm_bitmap.matrix(flip_matrix_vertically(x@index - 1L))
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.pixmapRGB <- function(x, ...,
                                   mode = c("darkness", "brightness"),
                                   threshold = 0.5) {
    mode <- match.arg(mode)
    red <- as.double(x@red)
    green <- as.double(x@green)
    blue <- as.double(x@blue)
    # make darker higher number
    grey <- switch(mode,
                   brightness = rgba2brightness(red, green, blue),
                   darkness = rgba2darkness(red, green, blue)
                   )
    m <- flip_matrix_vertically(matrix(as.matrix(grey >= threshold),
                                       nrow = x@size[1L], ncol = x@size[2L]))
    as_bm_bitmap.matrix(m)
}

#' @rdname as_bm_bitmap
#' @export
as_bm_bitmap.raster <- function(x, ...,
                                mode = c("alpha", "darkness", "brightness"),
                                threshold = 0.5) {
    mode <- match.arg(mode)
    as_bm_bitmap.bm_pixmap(as_bm_pixmap.raster(x),
                           mode = mode, threshold = threshold)
}
