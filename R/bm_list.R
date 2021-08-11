#' Bitmap list object
#'
#' `bm_list()` creates a bitmap list object.
#'
#' `bm_list()` is a list of [bm_bitmap()] objects with class \dQuote{bm_list}.
#' It is superclass of [bm_font()].
#' @section Supported S3 methods:
#'
#' * `as.list.bm_list()`
#' * Slicing with `[]` returns `bm_list()` objects.
#' * The `min()`, `max()`, and `range()` functions from the \dQuote{Summary}
#'   group of generic methods.
#'
#' @param ... [bm_bitmap()] objects, possibly named.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'
#'  gl <- font[c("U+0023", "U+0052", "U+0053", "U+0054", "U+0041", "U+0054", "U+0053")] # #RSTATS
#'  gl <- as_bm_list(gl)
#'  is_bm_list(gl)
#'
#' @return A named list with a \dQuote{bm_list} subclass.
#' @seealso [is_bm_list()], [as_bm_list()]
#' @export
bm_list <- function(...) {
    as_bm_list(list(...))
}

#' Test if the object is a bitmap glyph list object
#'
#' `is_bm_list()` returns `TRUE` for [bm_list()] objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  is_bm_font(font)
#' @seealso [bm_list()]
#' @export
is_bm_list <- function(x) {
    inherits(x, "bm_list")
}

validate_bm_list <- function(x) {
    if (!all(sapply(x, is_bm_bitmap)))
        stop("Some elements were not `bm_bitmap()` objects")
    invisible(NULL)
}

#' Modify bitmap lists
#'
#' `bm_lapply()` applies a function over a bitmap glyph list
#' and returns a modified bitmap glyph list.
#'
#' `bm_lapply()` is a wrapper around `base::lapply()` that
#' preserves the classes and metadata of the original bitmap glyph list.
#' @param X A bitmap glyph list object such as [bm_list()] or [bm_font()].
#' @param FUN A function that takes a [bm_bitmap()] object as its first argument
#'            and returns a [bm_bitmap()] object.
#' @param ... Additional arguments to pass to `FUN`.
#' @return A modified bitmap glyph list.
#' @seealso [base::lapply()], [bm_list()], [bm_font()], [bm_bitmap()]
#' @export
bm_lapply <- function(X, FUN, ...) { # nolint
    stopifnot(is_bm_list(X))
    l2 <- lapply(X, FUN, ...)
    class(l2) <- class(X)
    attr(l2, "comments") <- attr(X, "comments")
    attr(l2, "properties") <- attr(X, "properties")
    l2
}

#' @export
as.list.bm_list <- function(x, ...) {
    n <- names(x)
    attributes(x) <- NULL
    names(x) <- n
    x
}

#' @export
`[.bm_list` <- function(x, i) {
    l <- NextMethod()
    class(l) <- c("bm_list", class(l))
    l
}
