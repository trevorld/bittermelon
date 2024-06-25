#' Cast to a pixmap matrix object
#'
#' `as_bm_pixmap()` casts an object to a `[bm_pixmap()]` object.
#'
#' @param x an Object
#' @param ... Potentially passed to other methods e.g. `as_bm_pixmap.default()` passes `...` to [as.raster()].
#' @seealso [bm_pixmap()], [is_bm_pixmap()]
#' @return A [bm_pixmap()] object.
#' @examples
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' is_bm_pixmap(corn)
#' all.equal(corn, as_bm_pixmap(as.array(corn)))
#' all.equal(corn, as_bm_pixmap(as.raster(corn)))
#' if (requireNamespace("farver", quietly = TRUE)) {
#'   all.equal(corn, as_bm_pixmap(as.raster(corn, native = TRUE)))
#' }
#' if (requireNamespace("magick", quietly = TRUE)) {
#'   all.equal(corn, as_bm_pixmap(magick::image_read(corn)))
#' }
#'
#' if (requireNamespace("mazing", quietly = TRUE) &&
#'     cli::is_utf8_output() &&
#'     cli::num_ansi_colors() >= 8L) {
#'   pal <- grDevices::palette.colors()
#'   pm <- as_bm_pixmap(mazing::maze(24L, 32L),
#'                      start = "top", end = "bottom",
#'                      col = c(pal[6], "white", pal[7], pal[5]))
#'   pm <- bm_pad(pm, sides = 1L)
#'   print(pm, compress = "v", bg = "white")
#' }
#' if (requireNamespace("gridpattern", quietly = TRUE) &&
#'     cli::is_utf8_output() &&
#'     cli::num_ansi_colors() >= 256L) {
#'   s <- gridpattern::pattern_square(subtype = 8L, nrow = 8L, ncol = 50L)
#'   pm <- as_bm_pixmap(s, col = grDevices::rainbow(8L))
#'   print(pm, compress = "vertical")
#' }
#' @export
as_bm_pixmap <- function(x, ...) {
    UseMethod("as_bm_pixmap")
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.default <- function(x, ...) {
    as_bm_pixmap.raster(grDevices::as.raster(x, ...))
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.array <- function(x, ...) {
    as_bm_pixmap.raster(grDevices::as.raster(x, ...))
}

#' @param col Character vector of R color specifications.
#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.bm_bitmap <- function(x, ..., col = getOption("bittermelon.col", col_bitmap)) { # nolint
    cols <- col2hex(col)[as.integer(x) + 1L]
    m <- matrix(cols, nrow = nrow(x), ncol = ncol(x))
    class(m) <- c("bm_pixmap", "bm_matrix", class(m))
    m
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.bm_pixmap <- function(x, ...) {
    x
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.glyph_bitmap <- function(x, ..., col = getOption("bittermelon.col", col_bitmap)) {
    as_bm_pixmap.bm_bitmap(as_bm_bitmap.glyph_bitmap(x), col = col)
}

#' @inheritParams as_bm_bitmap
#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.grob <- function(x, ..., width = 16L, height = 16L,
                              png_device = NULL) {
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
    as_bm_pixmap.array(a)
}

#' @rdname as_bm_pixmap
#' @export
`as_bm_pixmap.magick-image` <- function(x, ...) {
    as_bm_pixmap.raster(grDevices::as.raster(x, ...))
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.matrix <- function(x, ...) {
    if (is.character(x)) {
        cols <- col2hex(as.character(x))
        m <- matrix(cols, nrow = nrow(x), ncol = ncol(x))
        class(m) <- c("bm_pixmap", "bm_matrix", class(m))
        m
    } else {
        grey <- as.double(x)
        m <- matrix(grDevices::rgb(grey, grey, grey),
                    nrow = nrow(x), ncol = ncol(x))
        as_bm_pixmap.matrix(flip_matrix_vertically(m))
    }
}

#' @rdname as_bm_pixmap
#' @inheritParams as_bm_bitmap
#' @export
as_bm_pixmap.maze <- function(x, ..., walls = FALSE, start = NULL, end = NULL,
                              solve = !is.null(start) && !is.null(end),
                              col = getOption("bittermelon.col", col_bitmap)) {
    as_bm_pixmap.bm_bitmap(as_bm_bitmap.maze(x, walls = walls,
                                             start = start, end = end, solve = solve),
                           col = col)
}

# #' @rdname as_bm_pixmap
# #' @export
# as_bm_pixmap.pattern_hex <- function(x, ..., col = getOption("bittermelon.col", col_bitmap)) {
#     as_bm_pixmap.bm_bitmap(as_bm_bitmap.pattern_hex(x), col = col)
# }

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.pattern_square <- function(x, ..., col = getOption("bittermelon.col", col_bitmap)) {
    as_bm_pixmap.bm_bitmap(as_bm_bitmap.pattern_square(x), col = col)
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.pattern_weave <- function(x, ..., col = getOption("bittermelon.col", col_bitmap)) {
    as_bm_pixmap.bm_bitmap(as_bm_bitmap.pattern_weave(x), col = col)
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.pixmapGrey <- function(x, ...) {
    grey <- as.double(x@grey)
    colors <- grDevices::rgb(grey, grey, grey)
    m <- flip_matrix_vertically(matrix(colors, nrow = x@size[1L], ncol = x@size[2L]))
    as_bm_pixmap.matrix(m)
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.pixmapIndexed <- function(x, ...) {
    bm <- as_bm_bitmap.pixmapIndexed(x)
    as_bm_pixmap(bm, col = x@col)
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.pixmapRGB <- function(x, ...) {
    red <- as.double(x@red)
    green <- as.double(x@green)
    blue <- as.double(x@blue)
    colors <- grDevices::rgb(red, green, blue)
    m <- flip_matrix_vertically(matrix(colors, nrow = x@size[1L], ncol = x@size[2L]))
    as_bm_pixmap.matrix(m)
}

# nativeRaster is same dimension as raster
# if you cast to vector by **rows** (instead of **columns**)
# but then build a matrix from this vector by **columns** (instead of **rows**)
# nativeRaster uses integer for colors...

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.nativeRaster <- function(x, ...) {
    if (nrow(x) > 0L && ncol(x) > 0L) {
        cols <- int2col(as.integer(x))
        m <- flip_matrix_vertically(matrix(cols, nrow = nrow(x), ncol = ncol(x), byrow = TRUE))
        class(m) <- c("bm_pixmap", "bm_matrix", class(m))
        m
    } else {
        as_bm_pixmap.matrix(matrix("#FFFFFF00", nrow = nrow(x), ncol = ncol(x)))
    }
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.pixeltrix <- function(x, ...) {
    bm <- as_bm_bitmap.pixeltrix(x)
    as_bm_pixmap.bm_bitmap(bm, col = attr(x, "colours"))
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.raster <- function(x, ...) {
    # Standardize all colors to #RRGGBBAA format
    if (nrow(x) > 0L && ncol(x) > 0L) {
        cols <- col2hex(as.character(flip_matrix_vertically(as.matrix(x))))
        m <- matrix(cols, nrow = nrow(x), ncol = ncol(x))
        class(m) <- c("bm_pixmap", "bm_matrix", class(m))
        m
    } else {
        as_bm_pixmap.matrix(matrix("#FFFFFF00", nrow = nrow(x), ncol = ncol(x)))
    }
}
