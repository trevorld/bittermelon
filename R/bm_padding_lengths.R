#' Compute bitmap padding lengths
#'
#' `bm_padding_lengths()` computes the padding lengths of a
#' target value for the top, right, bottom, and left sides of the bitmap.
#' If the entire bitmap is of the target value then the left/right and top/bottom
#' will simply split the width/height in half.
#'
#' @inheritParams bm_clamp
#' @param value The value of the \dQuote{padding} integer to compute lengths for.
#' @return If `bm_object` is a `bm_bitmap()` object then a integer vector of length four
#'         representing the padding lengths for the top, right, bottom, and left sides respectively.
#'         If `bm_object` is a `bm_list()` or `bm_font()` then a list of integer vectors of length four.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  # add a border to an "R"
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = c(".", "@"))
#'  print(bm_padding_lengths(capital_r))
#' @export
bm_padding_lengths <- function(bm_object, value = 0L) {
    stopifnot(is_bm_object(bm_object))
    if (is_bm_bitmap(bm_object)) {
        bm_padding_lengths_bitmap(bm_object, value = value)
    } else {
        lapply(bm_object, bm_padding_lengths_bitmap, value = value)
    }
}

bm_padding_lengths_bitmap <- function(bm_bitmap, value = 0L) { # nolint
    height <- nrow(bm_bitmap)
    width <- ncol(bm_bitmap)

    row_bool <- apply(bm_bitmap, 1, function(x) all(x == value))
    if (all(row_bool) || height == 0L || width == 0L) {
        top <- height %/% 2L
        bottom <- height - top
        right <- width %/% 2L
        left <- width - right
    } else {
        col_bool <- apply(bm_bitmap, 2, function(x) all(x == value))

        left <- 0L
        for (j in seq.int(width)) {
            if (col_bool[j])
                left <- left + 1L
            else
                break
        }
        right <- 0L
        for (j in rev(seq.int(width))) {
            if (col_bool[j])
                right <- right + 1L
            else
                break
        }
        bottom <- 0L
        for (i in seq.int(height)) {
            if (row_bool[i])
                bottom <- bottom + 1L
            else
                break
        }
        top <- 0L
        for (i in rev(seq.int(height))) {
            if (row_bool[i])
                top <- top + 1L
            else
                break
        }
    }
    return(c(top, right, bottom, left))
}
