#' Print bitmap objects
#'
#' `print.bm_bitmap()` prints a representation of bitmap objects to the terminal.
#' It is a wrapper around `format.bm_bitmap()` which converts bitmap objects
#' to a character vector.
#' `px_unicode` and `px_ascii` are builtin character vectors intended for use with the `px`
#' argument (the former contains Unicode \dQuote{Block Elements} while the latter is purely ASCII).
#' `px_auto()` chooses which character vector to use based on whether [cli::is_utf8_output()] is `TRUE` or not.
#'
#' @inheritSection bm_print Fonts and terminal settings
#'
#' @param x A `bm_bitmap()` object
#' @param ... Further arguments passed to or from other methods.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' bm_R <- font[[str2ucp("R")]]
#' print(bm_R)
#'
#' if (cli::is_utf8_output())
#'   print(bm_R, px = px_unicode, compress = "vertical")
#'
#' bm_8 <- font[[str2ucp("8")]]
#' bm_8_with_border <- bm_extend(bm_extend(bm_8, left = 1L),
#'                               sides = 1L, value = 2L)
#' print(bm_8_with_border, px = c(".", "@", "X"))
#'
#' if (cli::num_ansi_colors() >= 16L) {
#'   print(bm_8_with_border, px = " ",
#'         bg = c(cli::bg_br_white, cli::bg_blue, cli::bg_red))
#' }
#' @seealso [bm_bitmap()]
#' @return A character vector of the string representation (`print.bm_bitmap()` does this invisibly).
#'         As a side effect `print.bm_bitmap()` prints out the string representation to the terminal.
#' @export
print.bm_bitmap <- function(x, ...,
                            px = getOption("bittermelon.px", px_auto()),
                            fg = getOption("bittermelon.fg", FALSE),
                            bg = getOption("bittermelon.bg", FALSE),
                            compress = getOption("bittermelon.compress", "none"),
                            downscale = getOption("bittermelon.downscale", FALSE)) {
    x <- as_bm_bitmap(x)
    s <- format(x, ..., px = px, fg = fg, bg = bg,
                compress = compress, downscale = downscale)
    cat(s, sep = "\n")
    invisible(s)
}

#' @rdname print.bm_bitmap
#' @param px Character vector of the pixel to use for each integer value i.e.
#'              The first character for integer `0L`,
#'              the second character for integer `1L`, and so on.
#'              Will be recycled.
#' @param fg R color strings of foreground colors to use and/or cli ANSI style functions of class `cli_ansi_style`.
#'           `FALSE` (default) for no foreground colors.
#'           Will be recycled and passed to [cli::make_ansi_style()].
#' @param bg R color strings of background colors to use and/or cli ANSI style functions of class `cli_ansi_style`.
#'           `FALSE` (default) for no background colors.
#'           Will be recycled and passed to [cli::make_ansi_style()] with `bg = TRUE`.
#' @param compress If "none" (default) or "n" don't compress first with [bm_compress()].
#'                 Otherwise compress first with [bm_compress()] passing
#'                 the value of `compress` as its `direction` argument
#'                 (i.e. either "vertical" or "v", "horizontal" or "h",
#'                  OR "both" or "b").
#' @param downscale If `TRUE` and the printed bitmap will be wider than `getOption("width")`
#'                  then shrink the image to fit `getOption("width")` using [bm_downscale()].
#' @export
format.bm_bitmap <- function(x, ...,
                             px = getOption("bittermelon.px", px_auto()),
                             fg = getOption("bittermelon.fg", FALSE),
                             bg = getOption("bittermelon.bg", FALSE),
                             compress = getOption("bittermelon.compress", "none"),
                             downscale = getOption("bittermelon.downscale", FALSE)) {
    if (nrow(x) == 0L || ncol(x) == 0L)
        return(character(0L))

    direction <- match.arg(tolower(compress),
                           c("none", "n", "vertical", "v", "horizontal", "h", "both", "b"))
    direction <- substr(direction, 1L, 1L)

    if (downscale) {
        x <- downscale_for_terminal(x, direction, filter = "Point")
    }
    if (direction != "n") {
        x <- bm_compress(x, direction = direction)
    }
    n <- max(x) + 1L
    px <- rep_len(px, n)
    if (!isFALSE(fg)) {
        if (is.function(fg)) {
            fg <- list(fg)
        # Avoid {cli} converting to function if color string in `cli:::ansi_builtin_styles()`
        } else if (is.character(fg)) {
            fg <- col2hex(fg)
        }
        fgl <- lapply(fg, function(col) cli::make_ansi_style(col))
        if (length(fgl) == 1) {
            fgl <- rep_len(fgl, n)
        } else if (length(fgl) < n) {
            fgl <- rep(fgl, each = 20L, length.out = n)
        }
    } else {
        fgl <- rep_len(FALSE, n)
    }
    if (!isFALSE(bg)) {

        if (is.function(bg)) { 
            bg <- list(bg)
        # Avoid {cli} converting to function if color string in `cli:::ansi_builtin_styles()`
        } else if (is.character(bg)) {
            bg <- col2hex(bg)
        }
        bgl <- lapply(bg, function(col) cli::make_ansi_style(col, bg = TRUE))
        if (length(bgl) == 1) {
            bgl <- rep_len(bgl, n)
        } else if (length(bgl) < n) {
            bgl <- rep(bgl, each = 20L, length.out = n)
        }
    } else {
        bgl <- rep_len(FALSE, n)
    }
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
              " ", ".", ",", "_",
              "'", "[", "/", "d",
              "`", "\\", "]", "L",
              "^", "?", "P", "!")

#' @rdname print.bm_bitmap
#' @param unicode Character vector to use if [cli::is_utf8_output()] is `TRUE`.
#' @param ascii Character vector to use if [cli::is_utf8_output()] is `FALSE`.
#' @export
px_auto <- function(unicode = px_unicode, ascii = px_ascii) {
    if (cli::is_utf8_output())
        unicode
    else
        ascii
}
