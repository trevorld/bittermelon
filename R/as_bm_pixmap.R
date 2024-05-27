#' Cast to a pixmap matrix object
#'
#' `as_bm_pixmap()` casts an object to a `[bm_pixmap()]` object.
#'
#' @param x an Object
#' @param ... Potentially passed to other methods e.g. `as_bm_pixmap.default()` passes `...` to [as.raster()].
#' @seealso [bm_pixmap()], [is_bm_pixmap()]
#' @return A [bm_pixmap()] object.
#' @examples
#' img <- png::readPNG(system.file("img", "Rlogo.png", package="png"))
#' pm <- as_bm_pixmap(img)
#' plot(pm)
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

#' @param col Character vector of R color specifications.
#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.bm_bitmap <- function(x, ..., col = getOption("bittermelon.col", col_bitmap)) { # nolint
    col <- col2rrggbbaa(col)
    x <- apply(as.matrix(x), 2L, function(i) col[i + 1L])
    class(x) <- c("bm_pixmap", "bm_matrix", class(x))
    x
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.bm_pixmap <- function(x, ...) {
    x
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.matrix <- function(x, ...) {
    if (is.character(x)) {
        if (nrow(x) > 0L && ncol(x) > 0L)
            for (i in seq_len(nrow(x)))
                x[i, ] <- col2rrggbbaa(x[i, ])
        class(x) <- c("bm_pixmap", "bm_matrix", class(x))
        x
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
    stopifnot(requireNamespace("mazing", quietly = TRUE))
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
    stopifnot(requireNamespace("farver", quietly = TRUE))
    if (nrow(x) > 0L && ncol(x) > 0L) {
        cols <- col2rrggbbaa(farver::decode_native(as.integer(x)))
        m <- matrix(cols, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
        m <- m[seq.int(nrow(x), 1L), , drop = FALSE]
        class(m) <- c("bm_pixmap", "bm_matrix", class(m))
        m
    } else {
        as_bm_pixmap.matrix("#FFFFFF00", nrow = nrow(x), ncol = ncol(x))
    }
}

#' @rdname as_bm_pixmap
#' @export
as_bm_pixmap.raster <- function(x, ...) {
    # Standardize all colors to #RRGGBBAA format
    if (nrow(x) > 0L && ncol(x) > 0L) {
        x <- as.matrix(x)[seq.int(nrow(x), 1L), , drop = FALSE]
        for (i in seq_len(nrow(x)))
            x[i, ] <- col2rrggbbaa(x[i, ])
        class(x) <- c("bm_pixmap", "bm_matrix", class(x))
        x
    } else {
        as_bm_pixmap.matrix("#FFFFFF00", nrow = nrow(x), ncol = ncol(x))
    }
}
