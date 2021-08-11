#' Other Unicode utilities
#'
#' `ucp2label()` returns Unicode code point \dQuote{labels} as a character vector.
#' `is_combining_character()` returns `TRUE` if the character is a \dQuote{combining} character.
#'
#' @param x A character vector of Unicode code points.
#' @return `ucp2label()` returns a character vector of Unicode labels.
#'         `is_combining_character()` returns a logical vector.
#' @examples
#'   # Get the Unicode Code Point "label" for "R"
#'   ucp2label(str2ucp("R"))
#'
#'   is_combining_character(str2ucp("a"))
#'   is_combining_character("U+0300") # COMBINING GRAVE ACCENT
#' @seealso [hex2ucp()], [int2ucp()], [name2ucp()], and [str2ucp()] all return Unicode code points.
#' @rdname unicode_utilities
#' @export
ucp2label <- function(x) {
    Unicode::u_char_label(x)
}

#' @rdname unicode_utilities
#' @export
is_combining_character <- function(x) {
    Unicode::u_char_property(x, "Canonical_Combining_Class") > 0L
}
