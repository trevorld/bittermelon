#' Compute bitmap padding lengths
#'
#' `bm_padding_lengths()` computes the padding lengths of a
#' target value for the top, right, bottom, and left sides of the bitmap.
#' If the entire bitmap is of the target value then the left/right and top/bottom
#' will simply split the width/height in half.
#'
#' @inheritParams bm_extend
#' @param value The value of the \dQuote{padding} element to compute lengths for.
#' @return If `x` is a `bm_bitmap()` object then a integer vector of length four
#'         representing the padding lengths for the top, right, bottom, and left sides respectively.
#'         If `x` is a `bm_list()` or `bm_font()` then a list of integer vectors of length four.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' # add a border to an "R"
#' capital_r <- font[[str2ucp("R")]]
#' print(capital_r)
#' print(bm_padding_lengths(capital_r))
#' corn <- farming_crops_16x16()$corn$portrait
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn, bg = "cyan", compress = "v")
#' }
#' print(bm_padding_lengths(corn))
#' @export
bm_padding_lengths <- function(x, value) {
    UseMethod("bm_padding_lengths")
}

#' @rdname bm_padding_lengths
#' @export
bm_padding_lengths.bm_bitmap <- function(x, value = 0L) {
    bm_padding_lengths_bitmap(x, value = as.integer(value))
}

#' @rdname bm_padding_lengths
#' @export
bm_padding_lengths.bm_list <- function(x, ...) {
    lapply(x, bm_padding_lengths, ...)
}

#' @rdname bm_padding_lengths
#' @export
bm_padding_lengths.bm_pixmap <- function(x, value = col2hex("transparent")) {
    bm_padding_lengths_bitmap(x, value = col2hex(value))
}

#' @rdname bm_padding_lengths
#' @export
`bm_padding_lengths.magick-image` <- function(x, value = "transparent") {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    value <- col2hex(value)
    bm_padding_lengths_bitmap(`as_bm_pixmap.magick-image`(x), 
                              value = value)
}

#' @rdname bm_padding_lengths
#' @export
bm_padding_lengths.nativeRaster <- function(x, value = col2int("transparent")) {
    value <- int2col(as_native(value))
    bm_padding_lengths_bitmap(as_bm_pixmap.nativeRaster(x), 
                              value = value)
}

#' @rdname bm_padding_lengths
#' @export
bm_padding_lengths.raster <- function(x, value = "transparent") {
    bm_padding_lengths_bitmap(as_bm_pixmap.raster(x), 
                              value = col2hex(value))
}

bm_padding_lengths_bitmap <- function(bm_bitmap, value = 0L) { # nolint
    height <- nrow(bm_bitmap)
    width <- ncol(bm_bitmap)

    row_padding <- apply(bm_bitmap, 1, function(x) all(x == value))
    if (all(row_padding) || height == 0L || width == 0L) {
        top <- height %/% 2L
        bottom <- height - top
        right <- width %/% 2L
        left <- width - right
    } else {
        col_non_padding <- which(!apply(bm_bitmap, 2, function(x) all(x == value)))
        row_non_padding <- which(!row_padding)

        left <- col_non_padding[1L] - 1L
        right <- width - col_non_padding[length(col_non_padding)]
        bottom <- row_non_padding[1L] - 1L
        top <- height - row_non_padding[length(row_non_padding)]
    }
    return(c(top = top, right = right, bottom = bottom, left = left))
}
