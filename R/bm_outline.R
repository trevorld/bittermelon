#' Compute "outline" bitmap of a bitmap
#'
#' `bm_outline()` returns a bitmap that is just the \dQuote{outline}
#' of another bitmap.
#' @inheritParams bm_clamp
#' @param bg Bitmap \dQuote{background} value.
#' @param value Bitmap \dQuote{color} value for the outline.
#' @inherit bm_clamp return
#' @examples
#' square <- bm_bitmap(matrix(1L, nrow = 16L, ncol = 16L))
#' square_outline <- bm_outline(square)
#' print(square_outline)
#'
#' if (require(grid) && capabilities("png")) {
#'   circle <- as_bm_bitmap(circleGrob(), width=16, height=16)
#'   circle_outline <- bm_outline(circle)
#'   print(circle_outline)
#' }
#'
#' corn <- farming_crops_16x16()$corn$portrait
#' corn_outline <- bm_outline(corn, "magenta")
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_outline, bg = "white")
#' }
#' @export
bm_outline <- function(x, value, bg) {
    UseMethod("bm_outline")
}

#' @rdname bm_outline
#' @export
bm_outline.bm_bitmap <- function(x, value = 1L, bg = 0L) {
    bm_outline_bitmap(x, value = value, bg = bg)
}

#' @rdname bm_outline
#' @export
bm_outline.bm_list <- function(x, ...) {
    bm_lapply(x, bm_outline, ...)
}

#' @rdname bm_outline
#' @export
bm_outline.bm_pixmap <- function(x, value = col2hex("black"), bg = col2hex("transparent")) {
    value <- col2hex(value)
    bg <- col2hex(bg)
    bm_outline_bitmap(x, value = value, bg = bg)
}

#' @rdname bm_outline
#' @export
`bm_outline.magick-image` <- function(x, value = "black", bg = "transparent") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    value <- col2hex(value)
    bg <- col2hex(bg)
    pm <- bm_outline_bitmap(as_bm_pixmap(x), value = value, bg = bg)
    magick::image_read(pm)
}

#' @rdname bm_outline
#' @export
bm_outline.nativeRaster <- function(x, value = col2int("black"), bg = col2int("transparent")) {
    value <- int2col(as_native(value))
    bg <- int2col(as_native(bg))
    pm <- bm_outline_bitmap(as_bm_pixmap(x), value = value, bg = bg)
    as.raster(pm, native = TRUE)
}

#' @rdname bm_outline
#' @export
bm_outline.raster <- function(x, value = "black", bg = "transparent") {
    value <- col2hex(value)
    bg <- col2hex(bg)
    pm <- bm_outline_bitmap(as_bm_pixmap(x), value = value, bg = bg)
    as.raster(pm)
}

bm_outline_bitmap <- function(x, value = 1L, bg = 0L) {
    if (nrow(x) <= 2L || ncol(x) <= 2L)
        return(x)

    outline <- x
    for (i in 2:(nrow(x) - 1L)) {
        for (j in 2:(ncol(x) - 1L)) {
            neighbors <- x[i, c(j - 1, j + 1)]
            neighbors <- c(neighbors, x[c(i - 1, i + 1), j])
            if (all(neighbors != bg))
                outline[i, j] <- bg
        }
    }
    outline[which(as.logical(outline != bg))] <- value
    outline
}
