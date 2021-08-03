#' S3 Ops group generic methods for bitmaps
#'
#' The S3 Ops group generic methods for `bm_bitmap()` objects
#' are simply the result of the generic integer matrix method
#' cast back to a binary `bm_bitmap()` object (which
#' is an integer matrix of ones and zeros).
#' Since [base::which()] does not automatically cast
#' its argument to a logical value we also redefine it as a generic
#' and besides a default method which simply calls `base:which()` we
#' offer a `which.bm_bitmap()` method that first
#' casts the bitmap to logical before calling `base::which()`.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(!capital_r, px = px_ascii)
#' capital_b <- font[[str2ucp("B")]]
#' print(capital_r & capital_b, px = px_ascii)
#' print(capital_r | capital_b, px = px_ascii)
#' print(capital_r + 1L, px = px_ascii)
#' print(capital_r + 1L > 1L, px = px_ascii)
#' which(capital_r > 0L)
#'
#' @inheritParams base::Ops
#' @rdname Ops.bm_bitmap
#' @export
Ops.bm_bitmap <- function(e1, e2) {
   as_bm_bitmap(NextMethod())
}

#' @inheritParams base::which
#' @rdname Ops.bm_bitmap
#' @export
which <- function(x, arr.ind = FALSE, useNames = TRUE) { # nolint
    UseMethod("which")
}

#' @rdname Ops.bm_bitmap
#' @export
which.default <- function(x, arr.ind = FALSE, useNames = TRUE) { # nolint
    base::which(x, arr.ind, useNames)
}

#' @rdname Ops.bm_bitmap
#' @export
which.bm_bitmap <- function(x, arr.ind = FALSE, useNames = TRUE) { # nolint
    base::which(as.logical(x))
}

#' Extract or replace parts of a bitmap
#'
#' `[.bm_bitmap()` is defined so that it returns a `bm_bitmap()` object
#' (if the value is a matrix) and `[<-.bm_bitmap()` casts
#' any replacement values as integers.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r[4:14,2:8], px = px_ascii)
#'  capital_r[11:13,3:5] <- 2L
#'  print(capital_r, px = px_ascii)
#' @param x [bm_bitmap()] object
#' @param i,j indices specifying elements to extract or replace.
#'            See [base::`[()`] for more information.
#' @param ... Passed to [base::`[()`].
#' @param drop If `TRUE` the result is coerced to a integer vector.
#' @param value Replacement value
#' @rdname Extract.bm_bitmap
#' @export
`[.bm_bitmap` <- function(x, i, j, ..., drop = TRUE) {
    v <- NextMethod()
    if (is.matrix(v) && !is_bm_bitmap(v))
        class(v) <- c("bm_bitmap", class(v))
    v
}

#' @rdname Extract.bm_bitmap
#' @export
`[<-.bm_bitmap` <- function(x, i, j, ..., value) { # nolint
    value <- as.integer(value)
    x <- NextMethod()
    x
}
