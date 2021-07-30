#' Get Unicode code points
#'
#' `code_point()` and `code_point_from_name()` return Unicode code points.
#'
#' `code_point()` is an alias for `as.character(Unicode::as.u_char(x))`.
#' `code_point_from_name(x)` is an alias for `as.character(Unicode::u_char_from_name(x))`.
#' The names of `bm_font()` objects must be character vectors and not `u_char` objects.
#' @param x R objects coercible to the respective Unicode character data types.
#'          See [Unicode::as.u_char()] for `code_point()` and
#'          [Unicode::u_char_from_name()] for `code_point_from_name()`.
#' @return A character vector of Unicode code points.
#' @examples
#'   code_point("20")
#'   code_point("U+0020")
#'   code_point("0x0020")
#'   code_point_from_name("SPACE")
#'   code_point_from_name("LATIN CAPITAL LETTER R")
#' @export
code_point <- function(x) {
    x <- as.character(Unicode::as.u_char(x))
    x <- ifelse(x == "<NA>", NA_character_, x)
    x
}

#' @rdname code_point
#' @export
code_point_from_name <- function(x) {
    as.character(Unicode::u_char_from_name(x))
}
