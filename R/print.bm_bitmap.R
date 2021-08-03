#' Print bitmap objects
#'
#' `print.bm_bitmap()` prints a representation of bitmap objects to the terminal.
#' It is a wrapper around `format.bm_bitmap()` which converts bitmap objects
#' to a character vector.
#' `px_unicode` and `px_ascii` are builtin character vectors intended for use with the `px`
#' argument (the former contains Unicode \dQuote{Block Elements} while the latter is purely ASCII).
#'
#' @param x A `bm_bitmap()` object
#' @param ... Further arguments passed to or from other methods.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   bm_R <- font[[str2ucp("R")]]
#'   print(bm_R, px = c(" ", "#"))
#'
#'   bm_8 <- font[[str2ucp("8")]]
#'   bm_8_with_border <- bm_extend(bm_extend(bm_8, left = 1L),
#'                                 sides = 1L, value = 2L)
#'   print(bm_8_with_border, px = c(".", "@", "X"))
#'
#'   if (require("crayon") && crayon::has_color()) {
#'     print(bm_8_with_border, px = " ", bg = c("white", "blue", "red"))
#'   }
#' @seealso [bm_bitmap()]
#' @return A character vector of the string representation (`print.bm_bitmap()` does this invisibly).
#'         As a side effect `print.bm_bitmap()` prints out the string representation to the terminal.
#' @usage \method{print}{bm_bitmap}(x, ..., px = px_unicode, fg = FALSE, bg = FALSE)
#' @export
print.bm_bitmap <- function(x, ..., px = px_unicode, fg = FALSE, bg = FALSE) {
    x <- as_bm_bitmap(x)
    s <- format(x, ..., px = px, fg = fg, bg = bg)
    cat(s, sep = "\n")
    invisible(s)
}

#' @usage \method{format}{bm_bitmap}(x, ..., px = px_unicode, fg = FALSE, bg = FALSE)
#' @rdname print.bm_bitmap
#' @param px Character vector of the pixel to use for each integer value i.e.
#'              The first character for integer `0L`,
#'              the second character for integer `1L`, and so on.
#'              Will be recycled.
#' @param fg R color strings of foreground colors to use.
#'           Requires suggested package `crayon`.
#'           `FALSE` (default) for no foreground colors.
#'           Will be recycled.
#' @param bg R color strings of background colors to use.
#'           Requires suggested package `crayon`.
#'           `FALSE` (default) for no background colors.
#'           Will be recycled.
#' @export
format.bm_bitmap <- function(x, ...,
                                   px = px_unicode,
                                   fg = FALSE, bg = FALSE) {
    x <- as_bm_bitmap(x)
    if (nrow(x) == 0L) return(character(0))
    n <- max(x) + 1L
    px <- rep_len(px, n)
    fgl <- as.list(fg)
    if (is.character(fg)) {
        stopifnot(requireNamespace("crayon", quietly = TRUE))
        fgl <- lapply(fgl, function(col) crayon::make_style(col))
    }
    fgl <- rep_len(fgl, n)
    bgl <- as.list(bg)
    if (is.character(bg)) {
        stopifnot(requireNamespace("crayon", quietly = TRUE))
        bgl <- lapply(bgl, function(col) crayon::make_style(col, bg = TRUE))
    }
    bgl <- rep_len(bgl, n)
    l <- sapply(seq_len(nrow(x)),
                function(i) bm_bitmap_row_to_string(x[i, ], px, fgl, bgl))
    rev(l)
}

bm_bitmap_row_to_string <- function(row, px, fgl, bgl) {
    l <- lapply(row, function(i) int_to_char(i, px, fgl, bgl))
    do.call(paste0, l)
}

int_to_char <- function(i, px, fgl, bgl) {
    char <- px[i + 1L]
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

#' @rdname print.bm_bitmap
#' @export
px_unicode <- c("\u2591", "\u2588", "\u2592", "\u2593",
                " ", "\u2597", "\u2596", "\u2584",
                "\u259d", "\u2590", "\u259e", "\u259f",
                "\u2598", "\u259a", "\u258c", "\u2599",
                "\u2580", "\u259c", "\u259b", "+")

#' @rdname print.bm_bitmap
#' @export
px_ascii <- c("-", "@", "+", "#",
              " ", "q", ",", "_",
              "'", "[", "/", "d",
              "`", "\\", "]", "L",
              "^", "?", "P", "!")
