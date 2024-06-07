#' Expand bitmaps by repeating each row and/or column
#'
#' `bm_expand()` expands bitmap(s) by repeating each row and/or column
#' an indicated number of times.
#'
#' @inheritParams bm_clamp
#' @inherit bm_clamp return
#' @param width An integer of how many times to repeat each column.
#' @param height An integer of how many times to repeat each row.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r)
#'  print(bm_expand(capital_r, 2L),
#'        px = px_ascii)
#'  print(bm_expand(capital_r, width = 1L, height = 2L),
#'        px = px_ascii)
#'  print(bm_expand(capital_r, width = 2L, height = 1L),
#'        px = px_ascii)
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' corn_2x <- bm_expand(corn, 2L)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_2x, compress = "v")
#' }
#' @seealso [bm_extend()] (and [bm_resize()] which makes larger bitmaps
#'           by adding pixels to their sides.
#' @export
bm_expand <- function(x, width = 1L, height = width) {
    UseMethod("bm_expand")
}

#' @rdname bm_expand
#' @export
bm_expand.bm_bitmap <- function(x, width = 1L, height = width) {
    if (nrow(x) == 0L || ncol(x) == 0L || width == 0L || height == 0L) {
        nr <- height * nrow(x)
        nc <- width * ncol(x)
        return(bm_bitmap(matrix(integer(), nrow = nr, ncol = nc)))
    } else {
        bm_expand_bitmap(x, width = width, height = height)
    }
}

#' @rdname bm_expand
#' @export
bm_expand.bm_list <- function(x, ...) {
    bm_lapply(x, bm_expand, ...)
}

#' @rdname bm_expand
#' @export
bm_expand.bm_pixmap <- function(x, width = 1L, height = width) {
    if (nrow(x) == 0L || ncol(x) == 0L || height == 0L || width == 0L) {
        nr <- height * nrow(x)
        nc <- width * ncol(x)
        return(bm_pixmap(matrix(character(), nrow = nr, ncol = nc)))
    } else {
        bm_expand_bitmap(x, width = width, height = height)
    }
}

#' @rdname bm_expand
#' @export
`bm_expand.magick-image` <- function(x, width = 1L, height = width) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    pm <- as_bm_pixmap(x)
    pm <- bm_expand(pm, width = width, height = height)
    magick::image_read(pm)
}

#' @rdname bm_expand
#' @export
bm_expand.nativeRaster <- function(x, width = 1L, height = width) {
    pm <- as_bm_pixmap(x)
    pm <- bm_expand(pm, width = width, height = height)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_expand
#' @export
bm_expand.raster <- function(x, width = 1L, height = width) {
    if (nrow(x) == 0L || ncol(x) == 0L || height == 0L || width == 0L) {
        nr <- height * nrow(x)
        nc <- width * ncol(x)
        return(as.raster(matrix(character(), nrow = nr, ncol = nc)))
    } else {
        # The `height` logic is a bit different from `bm_expand_bitmap()`
        x <- as.matrix(x)
        if (width != 1L) {
            l <- lapply(seq_len(ncol(x)),
                        function(j) x[, j, drop = FALSE])
            l <- rep(l, each = width)
            x <- do.call(cbind, l)
        }
        if (height != 1L) {
            l <- lapply(seq_len(nrow(x)),
                        function(i) x[i, , drop = FALSE])
            l <- rep(l, each = height)
            x <- do.call(rbind, l)
        }
        as.raster(x)
    }
}

bm_expand_bitmap <- function(x, width = 1L, height = width) {
    if (width != 1L) {
        l <- lapply(seq_len(ncol(x)),
                    function(j) x[, j, drop = FALSE])
        l <- rep(l, each = width)
        x <- do.call(cbind, l)
    }
    if (height != 1L) {
        l <- lapply(seq.int(nrow(x), 1L, -1L),
                    function(i) x[i, , drop = FALSE])
        l <- rep(l, each = height)
        x <- do.call(rbind, l)
    }
    x
}
