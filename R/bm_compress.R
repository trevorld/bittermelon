#' Compress bitmaps by a factor of two
#'
#' Compresses [bm_bitmap()] objects by a factor of two by re-mapping to a \dQuote{block elements} scheme.
#' For pixmap objects like [bm_pixmap()] we simply shrink the pixmap by a factor of two using [bm_distort()].
#'
#' Depending on `direction` we shrink the bitmaps height and/or width by
#' a factor of two and re-encode pairs/quartets of pixels to a \dQuote{block elements} scheme.
#' If necessary we pad the right/bottom of the bitmap(s) by
#' a pixel. For each pair/quartet we determine the most-common non-zero element
#' and map them to a length twenty set of integers representing the \dQuote{block
#' elements} scheme.  For integers greater than zero we map it to higher twenty
#' character sets i.e. `1`'s get mapped to 0:19, `2`'s get mapped to 20:39, `3`'s
#' get mapped to 40:59, etc.  Using the default `px_unicode` will give you the exact
#' matching \dQuote{Block Elements} glyphs while `px_ascii` gives the closest ASCII approximation.
#' Hence `print.bm_bitmap()` should produce reasonable results for compressed bitmaps if
#' either of them are used as the `px` argument.
#' @inheritParams bm_clamp
#' @inheritParams bm_distort
#' @inheritParams bm_flip
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' r <- font[[str2ucp("R")]]
#' print(r)
#' print(bm_compress(r, "vertical"))
#' print(bm_compress(r, "horizontal"))
#' print(bm_compress(r, "both"))
#'
#' img <- png::readPNG(system.file("img", "Rlogo.png", package="png"))
#' logo <- as_bm_pixmap(img)
#' if (cli::is_utf8_output() && 
#'     cli::num_ansi_colors() > 256L &&
#'     requireNamespace("magick", quietly = TRUE)) {
#'   logo_c <- bm_compress(pm, "both", filter = NULL)
#'   print(logo_c, compress = "v")
#' }
#' @inherit bm_clamp return
#' @seealso See <https://en.wikipedia.org/wiki/Block_Elements> for more info on the Unicode Block Elements block.
#' @export
bm_compress <- function(x, direction = "vertical", ...) {
    UseMethod("bm_compress")
}

#' @rdname bm_compress
#' @export
bm_compress.bm_bitmap <- function(x, direction = "vertical", ...) {
    direction <- match.arg(tolower(direction),
                           c("vertical", "v", "horizontal", "h", "both", "b"))
    direction <- substr(direction, 1L, 1L)
    bm_compress_bitmap(x, direction = direction)
}

#' @rdname bm_compress
#' @export
bm_compress.bm_pixmap <- function(x, direction = "vertical", ..., filter = "Point") {
    bm_compress_pixmap(x, direction, filter)
}

#' @rdname bm_compress
#' @export
`bm_compress.magick-image` <- function(x, direction = "vertical", ..., filter = "Point") {
    bm_compress_pixmap(x, direction, filter)
}

#' @rdname bm_compress
#' @export
bm_compress.nativeRaster <- function(x, direction = "vertical", ..., filter = "Point") {
    bm_compress_pixmap(x, direction, filter)
}

#' @rdname bm_compress
#' @export
bm_compress.raster <- function(x, direction = "vertical", ..., filter = "Point") {
    bm_compress_pixmap(x, direction, filter)
}

bm_compress_pixmap <- function(x, direction, filter) {
    direction <- match.arg(tolower(direction),
                           c("vertical", "v", "horizontal", "h", "both", "b"))
    direction <- substr(direction, 1L, 1L)
    switch(direction,
           v = bm_distort(x, width = bm_widths(x),
                          height = bm_heights(x) / 2L,
                          filter = filter),
           h = bm_distort(x, width = bm_widths(x) / 2L,
                          height = bm_heights(x),
                          filter = filter),
           b = bm_distort(x, width = bm_widths(x) / 2L, 
                          height = bm_heights(x) / 2L,
                          filter = filter)
           )
}

#' @rdname bm_compress
#' @export
bm_compress.bm_list <- function(x, ...) {
    bm_lapply(x, bm_compress, ...)
}

bm_compress_bitmap <- function(x, direction = "v") {
    if (ncol(x) %% 2L == 1L)
        x <- bm_extend(x, right = 1L)
    if (nrow(x) %% 2L == 1L)
        x <- bm_extend(x, bottom = 1L)
    switch(direction,
           b = bm_compress_both(x),
           h = bm_compress_horizontal(x),
           v = bm_compress_vertical(x))
}

bm_compress_both <- function(x) {
    m <- matrix(0L, nrow = nrow(x) / 2L, ncol = ncol(x) / 2L)
    for (i in seq_len(nrow(m))) {
        for (j in seq_len(ncol(m))) {
            pixels <- x[seq(2L * (i - 1L) + 1L, length.out = 2L),
                             seq(2L * (j - 1L) + 1L, length.out = 2L)]
            int <- mode_int(pixels)
            pixels[which(as.logical(pixels > 0))] <- 1L
            pixels <- paste(as.character(pixels), collapse = "")
            new <- switch(pixels,
                          "1111" = 1L,
                          "0000" = 4L,
                          "0010" = 5L,
                          "1000" = 6L,
                          "1010" = 7L,
                          "0001" = 8L,
                          "0011" = 9L,
                          "1001" = 10L,
                          "1011" = 11L,
                          "0100" = 12L,
                          "0110" = 13L,
                          "1100" = 14L,
                          "1110" = 15L,
                          "0101" = 16L,
                          "0111" = 17L,
                          "1101" = 18L,
                          stop("Can't handle this case"))
            m[i, j] <- new + (int - 1L) * 20L
        }
    }
    bm_bitmap(m)
}
bm_compress_vertical <- function(x) {
    m <- matrix(0L, nrow = nrow(x) / 2L, ncol = ncol(x))
    for (i in seq_len(nrow(m))) {
        for (j in seq_len(ncol(m))) {
            pixels <- x[seq(2L * (i - 1L) + 1L, length.out = 2L), j]
            int <- mode_int(pixels)
            pixels[pixels > 0] <- 1L
            pixels <- paste(as.character(pixels), collapse = "")
            new <- switch(pixels,
                          "00" = 4L,
                          "01" = 16L,
                          "10" = 7L,
                          "11" = 1L,
                          stop("Can't handle this case"))
            m[i, j] <- new + (int - 1L) * 20L
        }
    }
    bm_bitmap(m)
}
bm_compress_horizontal <- function(x) {
    m <- matrix(0L, nrow = nrow(x), ncol = ncol(x) / 2L)
    for (i in seq_len(nrow(m))) {
        for (j in seq_len(ncol(m))) {
            pixels <- x[i, seq(2L * (j - 1L) + 1L, length.out = 2L)]
            int <- mode_int(pixels)
            pixels[pixels > 0] <- 1L
            pixels <- paste(as.character(pixels), collapse = "")
            new <- switch(pixels,
                          "00" = 4L,
                          "01" = 9L,
                          "10" = 14L,
                          "11" = 1L,
                          stop("Can't handle this case"))
            m[i, j] <- new + (int - 1L) * 20L
        }
    }
    bm_bitmap(m)
}

mode_int <- function(v) {
   which.max(tabulate(v))
}
