#' Compress bitmaps using a "block elements" scheme
#'
#' Compress bitmaps by a factor of two by re-mapping to a \dQuote{block elements} scheme.
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
#' @inheritParams bm_flip
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   r <- font[[str2ucp("R")]]
#'   print(r, px = px_ascii)
#'   print(bm_compress(r, "vertical"), px = px_ascii)
#'   print(bm_compress(r, "horizontal"), px = px_ascii)
#'   print(bm_compress(r, "both"), px = px_ascii)
#' @inherit bm_clamp return
#' @seealso See <https://en.wikipedia.org/wiki/Block_Elements> for more info on the Unicode Block Elements block.
#' @export
bm_compress <- function(bm_object, direction = "vertical") {
    direction <- match.arg(tolower(direction),
                           c("vertical", "v", "horizontal", "h", "both", "b"))
    direction <- substr(direction, 1L, 1L)
    modify_bm_bitmaps(bm_object, bm_compress_bitmap, direction = direction)
}

bm_compress_bitmap <- function(bitmap, direction = "v") {
    if (ncol(bitmap) %% 2L == 1L)
        bitmap <- bm_extend(bitmap, right = 1L)
    if (nrow(bitmap) %% 2L == 1L)
        bitmap <- bm_extend(bitmap, bottom = 1L)
    switch(direction,
           b = bm_compress_both(bitmap),
           h = bm_compress_horizontal(bitmap),
           v = bm_compress_vertical(bitmap))
}

bm_compress_both <- function(bitmap) {
    m <- matrix(0L, nrow = nrow(bitmap) / 2L, ncol = ncol(bitmap) / 2L)
    for (i in seq_len(nrow(m))) {
        for (j in seq_len(ncol(m))) {
            pixels <- bitmap[seq(2L * (i - 1L) + 1L, length.out = 2L),
                             seq(2L * (j - 1L) + 1L, length.out = 2L)]
            int <- mode_int(pixels)
            pixels[which(pixels > 0)] <- 1L
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
bm_compress_vertical <- function(bitmap) {
    m <- matrix(0L, nrow = nrow(bitmap) / 2L, ncol = ncol(bitmap))
    for (i in seq_len(nrow(m))) {
        for (j in seq_len(ncol(m))) {
            pixels <- bitmap[seq(2L * (i - 1L) + 1L, length.out = 2L), j]
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
bm_compress_horizontal <- function(bitmap) {
    m <- matrix(0L, nrow = nrow(bitmap), ncol = ncol(bitmap) / 2L)
    for (i in seq_len(nrow(m))) {
        for (j in seq_len(ncol(m))) {
            pixels <- bitmap[i, seq(2L * (j - 1L) + 1L, length.out = 2L)]
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
