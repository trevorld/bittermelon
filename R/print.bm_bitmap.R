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
#'   if (require("crayon")) {
#'     print(bm_8_with_border, labels = " ", bg = c("white", "blue", "red"))
#'   }
#' @seealso [bm_bitmap()]
#' @return A character vector of the string representation (`print.bm_bitmap()` does this invisibly).
#'         As a side effect `print.bm_bitmap()` prints out the string representation to the terminal.
#' @usage \method{print}{bm_bitmap}(x, ..., labels = c("\u2591", "\u2588", "\u2592"), fg = FALSE, bg = FALSE)
#' @export
print.bm_bitmap <- function(x, ..., labels = c("\u2591", "\u2588", "\u2592"), fg = FALSE, bg = FALSE) {
    x <- as_bm_bitmap(x)
    s <- as.character(x, ..., labels = labels, fg = fg, bg = bg)
    cat(s, sep = "\n")
    invisible(s)
}

#' @usage \method{as.character}{bm_bitmap}(x, ..., labels = c("\u2591", "\u2588", "\u2592"), fg = FALSE, bg = FALSE)
#' @rdname print.bm_bitmap
#' @param fg R color strings of foreground colors to use.
#'           Requires suggested package `crayon`.
#'           `FALSE` (default) for no foreground colors.
#' @param bg R color strings of background colors to use.
#'           Requires suggested package `crayon`.
#'           `FALSE` (default) for no background colors.
#' @export
as.character.bm_bitmap <- function(x, ...,
                                   labels = c("\u2591", "\u2588", "\u2592"),
                                   fg = FALSE, bg = FALSE) {
    x <- as_bm_bitmap(x)
    if (nrow(x) == 0L) return(character(0))
    max_integer <- max(x)
    if (length(labels) == 1L)
        labels <- rep_len(labels, max_integer + 1L)
    fgl <- as.list(fg)
    if (is.character(fg)) {
        stopifnot(requireNamespace("crayon", quietly = TRUE))
        fgl <- lapply(fgl, function(col) crayon::make_style(col))
    }
    bgl <- as.list(bg)
    if (is.character(bg)) {
        stopifnot(requireNamespace("crayon", quietly = TRUE))
        bgl <- lapply(bgl, function(col) crayon::make_style(col, bg = TRUE))
    }
    if (length(fgl) == 1L)
        fgl <- rep_len(fgl, max_integer + 1L)
    if (length(bgl) == 1L)
        bgl <- rep_len(bgl, max_integer + 1L)
    stopifnot((max_integer + 1L) <= length(labels))
    stopifnot((max_integer + 1L) <= length(fgl))
    stopifnot((max_integer + 1L) <= length(bgl))
    l <- sapply(seq_len(nrow(x)),
                function(i) bm_bitmap_row_to_string(x[i, ], labels, fgl, bgl))
    rev(l)
}

bm_bitmap_row_to_string <- function(row, labels, fgl, bgl) {
    l <- lapply(row, function(i) int_to_char(i, labels, fgl, bgl))
    do.call(paste0, l)
}

int_to_char <- function(i, labels, fgl, bgl) {
    char <- labels[i + 1L]
    fg_col <- fgl[[i + 1L]]
    if (is.function(fg_col)) {
        char <- fg_col(char)
    }
    bg_col <- bgl[[i + 1L]]
    if (is.function(bg_col)) {
        char <- bg_col(char)
    }
    char
}
