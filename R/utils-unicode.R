#' Other Unicode utilities
#'
#' `ucp2label()` returns Unicode code point \dQuote{labels} as a character vector.
#' `ucp_sort()` sorts Unicode code points.
#' `is_combining_character()` returns `TRUE` if the character is a \dQuote{combining} character.
#'
#' @param x A character vector of Unicode code points.
#' @param decreasing If `TRUE` do a decreasing sort.
#' @return `ucp2label()` returns a character vector of Unicode labels.
#'         `ucp_sort()` returns a character vector of Unicode code points.
#'         `is_combining_character()` returns a logical vector.
#' @examples
#'   # Get the Unicode Code Point "label" for "R"
#'   ucp2label(str2ucp("R"))
#'
#'   is_combining_character(str2ucp("a"))
#'   is_combining_character("U+0300") # COMBINING GRAVE ACCENT
#' @seealso [block2ucp()], [hex2ucp()], [int2ucp()], [name2ucp()], [range2ucp()], and [str2ucp()]
#'           all return Unicode code points.
#' @rdname unicode_utilities
#' @export
ucp2label <- function(x) {
    Unicode::u_char_label(x)
}

#' @rdname unicode_utilities
#' @export
ucp_sort <- function(x, decreasing = FALSE) {
    int2ucp(sort(as.integer(Unicode::as.u_char(x)), decreasing = decreasing))
}

#' @rdname unicode_utilities
#' @param pua_combining Additional Unicode code points to be considered
#'                      as a \dQuote{combining} character such as characters
#'                      defined in the Private Use Area (PUA) of a font.
#' @export
is_combining_character <- function(x, pua_combining = character(0)) {
    x <- (Unicode::u_char_property(x, "Canonical_Combining_Class") > 0L) |
        (Unicode::u_char_property(x, "General_Category") == "Me") |
        (x %in% pua_combining)
    vapply(x, isTRUE, logical(1))
}
