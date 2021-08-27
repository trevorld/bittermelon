#' Resize bitmaps by trimming and/or extending
#'
#' Trim and/or extend bitmaps to a desired height and/or width.
#'
#' This function is a convenience wrapper around [bm_trim()] and [bm_extend()].
#'
#' @inheritParams bm_extend
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  # add a border to an "R"
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = c("-", "#"))
#'  capital_r <- bm_resize(capital_r, width = 12L, height = 12L, vjust = "top")
#'  print(capital_r, px = c("-", "#"))
#' @seealso [bm_extend()], [bm_pad()], and [bm_trim()].
#' @inherit bm_clamp return
#' @export
bm_resize <- function(bm_object, value = 0L, # nolint
                      width = NULL, height = NULL,
                      hjust = "center-left", vjust = "center-top") {
    modify_bm_bitmaps(bm_object, bm_resize_bitmap, value = value,
                      width = width, height = height,
                      hjust = hjust, vjust = vjust)
}

bm_resize_bitmap <- function(bitmap, value = 0L,
                             width = NULL, height = NULL,
                             hjust = hjust, vjust = vjust) {
    if (!is.null(width) && ncol(bitmap) != width) {
        if (ncol(bitmap) < width)
            bitmap <- bm_extend(bitmap, value = value, width = width, hjust = hjust)
        else
            bitmap <- bm_trim(bitmap, width = width, hjust = hjust)
    }
    if (!is.null(height) && nrow(bitmap) != height) {
        if (nrow(bitmap) < height)
            bitmap <- bm_extend(bitmap, value = value, height = height, vjust = vjust)
        else
            bitmap <- bm_trim(bitmap, height = height, vjust = vjust)
    }
    bitmap
}
