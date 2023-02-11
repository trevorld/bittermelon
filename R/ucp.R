#' Get Unicode code points
#'
#' `hex2ucp()`, `int2ucp()`, `name2ucp()`, and `str2ucp()` return
#' Unicode code points as character vectors. `is_ucp()` returns
#' `TRUE` if a valid Unicode code point.
#'
#' `hex2ucp(x)` is a wrapper for `as.character(Unicode::as.u_char(x))`.
#' `int2ucp` is a wrapper for `as.character(Unicode::as.u_char(as.integer(x)))`.
#' `str2ucp(x)` is a wrapper for `as.character(Unicode::as.u_char(utf8ToInt(x)))`.
#' `name2ucp(x)` is a wrapper for `as.character(Unicode::u_char_from_name(x))`.
#' However missing values are coerced to `NA_character_` instead of `"<NA>"`.
#' Note the names of `bm_font()` objects must be character vectors as returned
#' by these functions and not `Unicode::u_char` objects.
#' @param x R objects coercible to the respective Unicode character data types.
#'          See [Unicode::as.u_char()] for `hex2ucp()` and `int2ucp()`,
#'          [base::utf8ToInt()] for `str2ucp()`,
#'          [Unicode::u_char_from_name()] for `name2ucp()`,
#'          [Unicode::as.u_char_range()] for `range2ucp()`,
#'          and [Unicode::u_blocks()] for `block2ucp()`.
#' @param omit_unnamed Omit control codes or unassigned code points
#' @inheritParams Unicode::u_char_from_name
#' @return A character vector of Unicode code points.
#' @examples
#'   # These are all different ways to get the same 'R' code point
#'   hex2ucp("52")
#'   hex2ucp(as.hexmode("52"))
#'   hex2ucp("0052")
#'   hex2ucp("U+0052")
#'   hex2ucp("0x0052")
#'   int2ucp(82) # 82 == as.hexmode("52")
#'   int2ucp("82") # 82 == as.hexmode("52")
#'   int2ucp(utf8ToInt("R"))
#'   ucp2label("U+0052")
#'   name2ucp("LATIN CAPITAL LETTER R")
#'   str2ucp("R")
#'
#'   # Potential gotcha as as.hexmode("52") == as.integer("82") == 52L
#'   all.equal(hex2ucp(52L), int2ucp(52L)) # TRUE
#'   all.equal(hex2ucp("52"), int2ucp("82")) # TRUE
#'   all.equal(hex2ucp("82"), int2ucp("82")) # FALSE
#'
#'   block2ucp("Basic Latin")
#'   block2ucp("Basic Latin", omit_unnamed = FALSE)
#'   range2ucp("U+0020..U+0030")
#'
#' @seealso [ucp2label()] and [is_combining_character()].
#' @rdname unicode_code_points
#' @export
hex2ucp <- function(x) {
    x <- as.character(Unicode::as.u_char(x))
    x <- ifelse(x == "<NA>", NA_character_, x)
    x
}

#' @rdname unicode_code_points
#' @export
int2ucp <- function(x) {
    x <- as.integer(x)
    hex2ucp(x)
}

#' @rdname unicode_code_points
#' @export
str2ucp <- function(x) {
    unlist(lapply(x, function(s) int2ucp(utf8ToInt(s))))
}

#' @rdname unicode_code_points
#' @export
name2ucp <- function(x, type = c("exact", "grep"), ...) {
    as.character(Unicode::u_char_from_name(toupper(x), type = type, ...))
}

#' @rdname unicode_code_points
#' @export
is_ucp <- function(x) {
    grepl("^U\\+[1-9A-F]*[0-9A-F]{4}$", x)
}

#' @rdname unicode_code_points
#' @export
block2ucp <- function(x, omit_unnamed = TRUE) {
    f <- function(x) as.character(Unicode::as.u_char(x))
    ucp <- unlist(lapply(Unicode::u_blocks(x), f))
    names(ucp) <- NULL

    if (omit_unnamed) {
        n <- Unicode::u_char_name(ucp)
        ucp <- ucp[which(!is.na(n) & n != "")]
    }
    ucp
}

#' @rdname unicode_code_points
#' @export
range2ucp <- function(x, omit_unnamed = TRUE) {
    r <- Unicode::as.u_char_range(x)
    ucp <- as.character(Unicode::as.u_char(r))
    if (omit_unnamed) {
        n <- Unicode::u_char_name(ucp)
        ucp <- ucp[which(!is.na(n) & n != "")]
    }
    ucp
}
