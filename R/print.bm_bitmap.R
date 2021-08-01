#' Print bitmap objects
#'
#' `print.bm_bitmap()` prints a representation of bitmap objects to the terminal.
#' It is a wrapper around `as.character.bm_bitmap()` which converts bitmap objects
#' to a character vector.
#'
#' @param x A `bm_bitmap()` object
#' @param ... Further arguments passed to or from other methods.
#' @param labels Character vector of the labels to use for each integer value i.e.
#'               The first character for integer `0L`,
#'               the second character for integer `1L`, and so on.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   bm_R <- font[[str2ucp("R")]]
#'   print(bm_R, labels = c(" ", "#"))
#'
#'   bm_8 <- font[[str2ucp("8")]]
#'   bm_8_with_border <- bm_extend(bm_extend(bm_8, left = 1L),
#'                                 sides = 1L, value = 2L)
#'   print(bm_8_with_border, labels = c(".", "@", "X"))
#'
#' @seealso [bm_bitmap()]
#' @return A character vector of the string representation (`print.bm_bitmap()` does this invisibly).
#'         As a side effect `print.bm_bitmap()` prints out the string representation to the terminal.
#' @usage \method{print}{bm_bitmap}(x, ..., labels = c("\u2591", "\u2588", "\u2592"))
#' @export
print.bm_bitmap <- function(x, ..., labels = c("\u2591", "\u2588", "\u2592")) {
    x <- as_bm_bitmap(x)
    s <- as.character(x, ..., labels = labels)
    cat(s, sep = "\n")
    invisible(s)
}

#' @usage \method{as.character}{bm_bitmap}(x, ..., labels = c("\u2591", "\u2588", "\u2592"))
#' @rdname print.bm_bitmap
#' @export
as.character.bm_bitmap <- function(x, ..., labels = c("\u2591", "\u2588", "\u2592")) {
    x <- as_bm_bitmap(x)
    if (nrow(x) == 0L) return(character(0))
    stopifnot(max(x) <= length(labels))
    l <- sapply(seq_len(nrow(x)),
                function(i) bm_bitmap_row_to_string(x[i, ], labels))
    rev(l)
}

bm_bitmap_row_to_string <- function(row, labels) {
    l <- lapply(row, function(i) labels[i + 1L])
    do.call(paste0, l)
}
