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
#' @return `[.bm_bitmap()` returns a `bm_bitmap()` object if the value is a matrix  and/or `drop` is `FALSE`
#'         otherwise it returns an integer matrix.
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
