#' Cast to a pixmap matrix object
#'
#' `as_bm_pixmap()` casts an object to a `[bm_pixmap()]` object.
#'
#' @param x an Object
#' @param ... Potentially passed to other methods e.g. `as_bm_pixmap.default()` passes `...` to [as.raster()].
#' @seealso [bm_pixmap()], [is_bm_pixmap()]
#' @return A [bm_pixmap()] object.
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
as_bm_pixmap.bm_bitmap <- function(x, ..., col = c("grey80", "black", "grey40")) { # nolint
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
#' @export
as_bm_pixmap.raster <- function(x, ...) {
    # Standardize all colors to #RRGGBBAA format
    if (nrow(x) > 0L && ncol(x) > 0L) {
        x <- as.matrix(x)[seq.int(nrow(x), 1L), , drop = FALSE]
        for (i in seq_len(nrow(x)))
            x[i, ] <- col2rrggbbaa(x[i, ])
    } else {
        x <- as.matrix(x)
    }
    class(x) <- c("bm_pixmap", "bm_matrix", class(x))
    x
}
