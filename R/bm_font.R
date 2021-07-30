#' Bitmap font object
#'
#' `bm_font()` creates a bitmap font object.
#'
#' `bm_font()` is a named list.
#' The names are of the form \dQuote{U+HHHH} or \dQuote{U+HHHHH}.
#' where the `H` are appropriate hexadecimal Unicode code points.
#' It is a subclass of [bm_list()].
#' @param x Named list of [bm_bitmap()] objects.
#'          Names must be coercible by [Unicode::as.u_char()].
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  is_bm_font(font)
#'
#'  # number of characters in font
#'  length(font)
#'
#'  # print out "R"
#'  R_code_point <- code_point_from_name("LATIN CAPITAL LETTER R") # U+0052
#'  print(font[[R_code_point]], labels = c(".", "#"))
#' @return A named list with a \dQuote{bm_font} subclass.
#' @seealso [is_bm_font()], [as_bm_font(), [code_point()]]
#' @export
bm_font <- function(x) {
    if (is_bm_font(x))
        x
    else
        as_bm_font(x)
}

#' Test if the object is a bitmap font object
#'
#' `is_bm_font()` returns `TRUE` for `bm_font` objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  is_bm_font(font)
#' @seealso [bm_font()]
#' @export
is_bm_font <- function(x) {
    inherits(x, "bm_font")
}

#' Coerce to bitmap font objects
#'
#' `as_bm_font()` turns an existing object into a `bm_font()` object.
#'
#' @param x An object that can reasonably be coerced to a `bm_font()` object.
#' @param ... Further arguments passed to or from other methods.
#' @return A `bm_font()` object.
#' @examples
#'   plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'   plus_sign[5L, 3:7] <- 1L
#'   plus_sign[3:7, 5L] <- 1L
#'   plus_sign_glyph <- bm_bitmap(plus_sign)
#'   plus_sign_code_point <- code_point_from_name("PLUS SIGN") # "U+002B"
#'
#'   space_glyph <- bm_bitmap(matrix(0L, nrow = 9L, ncol = 9L))
#'   space_code_point <- code_point_from_name("SPACE") # "U+0020"
#'
#'   l <- list()
#'   l[[plus_sign_code_point]] <- plus_sign_glyph
#'   l[[space_code_point]] <- space_glyph
#'   font <- as_bm_font(l)
#'   is_bm_font(font)
#'
#' @seealso [bm_font()]
#' @export
as_bm_font <- function(x, ...) {
    UseMethod("as_bm_font")
}

#' @rdname as_bm_font
#' @export
as_bm_font.default <- function(x, ...) {
    if (is_bm_font(x)) return(x)
    as_bm_font.list(as.list(x))
}

#' @rdname as_bm_font
#' @export
as_bm_font.list <- function(x, ...) {
    if (is_bm_font(x)) return(x)
    validate_bm_font(x)
    x <- as_bm_list(x)
    names(x) <- code_point(names(x))
    class(x) <- c("bm_font", class(x))
    x
}

validate_bm_font <- function(x) {
    validate_bm_list(x)
    if (is.null(names(x)) || any(names(x) == ""))
        stop("'x' must be a **named** list (with Unicode code point names)")
    codepoints <- code_point(names(x))
    if (any(is.na(codepoints)))
        stop("Some names were not coercible by `Unicode::as_u_char()`")
    invisible(NULL)
}
