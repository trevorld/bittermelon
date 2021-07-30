#' Get Unicode code points
#'
#' `code_point()`, `code_point_from_char()`, and `code_point_from_name()` return Unicode code points.
#'
#' `code_point()` is a wrapper for `as.character(Unicode::as.u_char())`.
#' `code_point_from_char()` is a wrapper for `as.character(Unicode::as.u_char(utf8ToInt()))`.
#' `code_point_from_name()` is a wrapper for `as.character(Unicode::u_char_from_name())`.
#' Note the names of `bm_font()` objects must be character vectors as returned
#' by these functions and not `u_char` objects.
#' @param x R objects coercible to the respective Unicode character data types.
#'          See [Unicode::as.u_char()] for `code_point()`,
#'          [base::utf8ToInt()] for `code_point_from_char()`, and
#'          [Unicode::u_char_from_name()] for `code_point_from_name()`.
#' @return A character vector of Unicode code points.
#' @examples
#'   # These are all different ways to get the 'R' code point
#'   code_point("52")
#'   code_point(as.hexmode("52"))
#'   code_point(82) # 82 == as.hexmode("52")
#'   code_point("U+0052")
#'   code_point("0x0052")
#'   code_point_from_char("R")
#'   code_point_from_name("LATIN CAPITAL LETTER R")
#' @export
code_point <- function(x) {
    x <- as.character(Unicode::as.u_char(x))
    x <- ifelse(x == "<NA>", NA_character_, x)
    x
}

#' @rdname code_point
#' @export
code_point_from_char <- function(x) {
    code_point(utf8ToInt(x))
}

#' @rdname code_point
#' @export
code_point_from_name <- function(x) {
    as.character(Unicode::u_char_from_name(x))
}
