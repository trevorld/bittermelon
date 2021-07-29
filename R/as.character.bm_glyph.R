#' Coerce bitmap glyph objects to character vector
#'
#' `as.character.bm_glyph()` converts bitmap glyph objects
#' to a character vector.
#'
#' @examples
#'   plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'   plus_sign[5L, 3:7] <- 1L
#'   plus_sign[3:7, 5L] <- 1L
#'   plus_sign <- bm_glyph(plus_sign)
#'   as.character(plus_sign, labels = c(" ", "#"))
#'
#'   space_glyph <- bm_glyph(matrix(0L, nrow = 8L, ncol = 8L))
#'   as.character(space_glyph, labels = ".")
#' @seealso [bm_glyph(), [print.bm_glyph()]
#' @param x A `bm_glyph()` object
#' @param ... Further arguments passed to or from other methods.
#' @param labels Character vector of the labels to use for each integer value i.e.
#'               The first character for integer `0L`,
#'               the second character for integer `1L`, and so on.
#' @return A character vector of the string representation.
#' @export
as.character.bm_glyph <- function(x, ..., labels = c(" ", "\u2588", "\u2592")) {
    x <- as_bm_glyph(x)
    stopifnot(max(x) <= length(labels))
    l <- sapply(seq_len(nrow(x)),
                function(i) bm_glyph_row_to_string(x[i, ], labels))
    rev(l)
}

bm_glyph_row_to_string <- function(row, labels) {
    l <- lapply(row, function(i) labels[i + 1L])
    do.call(paste0, l)
}
