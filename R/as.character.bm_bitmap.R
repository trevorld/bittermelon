#' Coerce bitmap objects to character vector
#'
#' `as.character.bm_bitmap()` converts bitmap objects
#' to a character vector.
#'
#' @examples
#'   plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'   plus_sign[5L, 3:7] <- 1L
#'   plus_sign[3:7, 5L] <- 1L
#'   plus_sign <- bm_bitmap(plus_sign)
#'   as.character(plus_sign, labels = c(" ", "#"))
#'
#'   space_glyph <- bm_bitmap(matrix(0L, nrow = 8L, ncol = 8L))
#'   as.character(space_glyph, labels = ".")
#' @seealso [bm_bitmap(), [print.bm_bitmap()]
#' @param x A `bm_bitmap()` object
#' @param ... Further arguments passed to or from other methods.
#' @param labels Character vector of the labels to use for each integer value i.e.
#'               The first character for integer `0L`,
#'               the second character for integer `1L`, and so on.
#' @return A character vector of the string representation.
#' @usage \method{as.character}{bm_bitmap}(x, ..., labels = c("\u2591", "\u2588", "\u2592"))
#' @export
as.character.bm_bitmap <- function(x, ..., labels = c("\u2591", "\u2588", "\u2592")) {
    x <- as_bm_bitmap(x)
    stopifnot(max(x) <= length(labels))
    l <- sapply(seq_len(nrow(x)),
                function(i) bm_bitmap_row_to_string(x[i, ], labels))
    rev(l)
}

bm_bitmap_row_to_string <- function(row, labels) {
    l <- lapply(row, function(i) labels[i + 1L])
    do.call(paste0, l)
}
