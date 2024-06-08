#' Coerce to bitmap list objects
#'
#' `as_bm_list()` turns an existing object into a [bm_list()] object.
#' In particular `as_bm_list.character()` turns a string into a bitmap list.
#'
#' @param x An object that can reasonably be coerced to a [bm_list()] object.
#' @param ... Further arguments passed to or from other methods.
#' @param font A [bm_font()] object that contains all the characters within `x`.
#' @return A [bm_list()] object.
#' @examples
#'   # as_bm_list.character()
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   bml <- as_bm_list("RSTATS", font = font)
#'   bml <- bm_extend(bml, sides = 1L, value = 0L)
#'   bml <- bm_extend(bml, sides = c(2L, 1L), value = 2L)
#'   bm <- do.call(cbind, bml)
#'   print(bm, px = c(" ", "#", "X"))
#' @seealso [bm_list()]
#' @export
as_bm_list <- function(x, ...) {
    UseMethod("as_bm_list")
}

#' @rdname as_bm_list
#' @export
as_bm_list.default <- function(x, ...) {
    as_bm_list.list(as.list(x, ...))
}

#' @rdname as_bm_list
#' @export
as_bm_list.bm_list <- function(x, ...) {
    x
}

#' @rdname as_bm_list
#' @param FUN Function to apply to every element of a list such
#'            as [as_bm_bitmap()] or [as_bm_pixmap()].
#' @export
as_bm_list.list <- function(x, ..., FUN = identity) {
    x <- lapply(x, FUN = FUN, ...)
    all_bm <- all(vapply(x, is_supported_bitmap, FUN.VALUE = logical(1L)))
    stopifnot(`Some elements were not supported bitmap objects` = all_bm)
    class(x) <- c("bm_list", class(x))
    x
}

#' @rdname as_bm_list
#' @export
as_bm_list.character <- function(x, ..., font = bm_font()) {
    x <- paste(x, collapse = "")
    if (nchar(x) == 0) return(bm_list())
    ucp <- str2ucp(x)
    bml <- as_bm_list(font[ucp])
    stopifnot(!any(sapply(bml, is.null)))
    bml
}

is_supported_bitmap <- function(x) {
    inherits(x, c("bm_bitmap", "bm_pixmap", "magick-image", "nativeRaster", "raster"))
}
