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
#' if (requireNamespace("mazing", quietly = TRUE)) {
#'   pm <- as_bm_pixmap(mazing::maze(32, 40), col = c("black", "white"))
#'   plot(pm)
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
        as_bm_pixmap.bm_bitmap(as_bm_bitmap.matrix(x))
    }
}

#' @rdname as_bm_pixmap
#' @param walls If `TRUE` the values of 1 denote the walls and the values of 0 denote the paths.
#' @param start,end If not `NULL` add the solution from `start` to `end` as value 2.  See [mazing::solve_maze()].
#' @export
as_bm_pixmap.maze <- function(x, ..., walls = FALSE, start = NULL, end = NULL,
                              col = getOption("bittermelon.col", col_bitmap)) {
    as_bm_pixmap.bm_bitmap(as_bm_bitmap.maze(x, walls = walls, start = start, end = end),
                           col = col)
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
