modify_bm_bitmaps <- function(x, fn, ...) {
    stopifnot(is_bm_list(x) || is_bm_matrix(x))
    if (is_bm_list(x)) {
        bm_lapply(x, fn, ...)
    } else {
        fn(x, ...)
    }
}

#' Colors to standardized hex strings
#'
#' `col2hex()` standardizes R color strings
#' into a unique RGBA hex string.
#' All fully transparent colors get standardized
#' to `"#FFFFFF00"`.
#'
#' @param x Color value as supported by [grDevices::col2rgb()].
#' @return A standardized RGBA hex string (as returned by [grDevices::rgb()]).
#' @examples
#' col2hex("red")
#' col2hex("green")
#' col2hex("blue")
#' col2hex("transparent")
#' col2hex(NA_character_)
#' col2hex("#00000000")
#' @export
col2hex <- function(x) {
    rgb <- grDevices::col2rgb(x, alpha = TRUE)
    which_transparent <- which(rgb[4, ] == 0L)
    if (length(which_transparent)) {
        rgb[1, which_transparent] <- 255L
        rgb[2, which_transparent] <- 255L
        rgb[3, which_transparent] <- 255L
    }
    grDevices::rgb(rgb[1, ], rgb[2, ], rgb[3, ], rgb[4, ],
                   maxColorValue = 255)
}

#' Color to (native) integer conversions
#'
#' `col2int()` converts color strings to (native) color integers.
#' `int2col()` converts (native) color integers to color strings.
#'
#' * Colors are also standardized by [col2hex()].
#' * Requires the [farver][farver::farver] package.
#' @param x Color value to convert.
#' @return `col2int()` returns an integer.  `int2col()` returns a (hex) color string.
#' @examples
#' if (requireNamespace("farver", quietly = TRUE)) {
#'   int2col(col2int("red"))
#' }
#' @export
col2int <- function(x) {
    stopifnot(requireNamespace("farver", quietly = TRUE))
    farver::encode_native(col2hex(x))
}

as_native <- function(x) {
    if (is.numeric(x)) {
        as.integer(x)
    } else {
        col2int(col2hex(x))
    }
}

#' @rdname col2int
#' @export
int2col <- function(x) {
    stopifnot(requireNamespace("farver", quietly = TRUE))
    col2hex(farver::decode_native(x))
}

`%||%` <- function(x, y) if (is.null(x)) y else x # nolint
