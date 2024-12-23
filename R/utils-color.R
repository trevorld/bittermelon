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
    if (requireNamespace("colorfast", quietly = TRUE)) {
        rgba <- colorfast::col_to_rgb(x)
    } else {
        rgba <- grDevices::col2rgb(x, alpha = TRUE)
    }
    which_transparent <- which(rgba[4, ] == 0L)
    if (length(which_transparent)) {
        rgba[1, which_transparent] <- 255L
        rgba[2, which_transparent] <- 255L
        rgba[3, which_transparent] <- 255L
    }
    grDevices::rgb(rgba[1, ], rgba[2, ], rgba[3, ], rgba[4, ],
                   maxColorValue = 255)
}

#' Color to (native) integer conversions
#'
#' `col2int()` converts color strings to (native) color integers.
#' `int2col()` converts (native) color integers to color strings.
#'
#' * Colors are also standardized by [col2hex()].
#' * Requires either the [colorfast](https://cran.r-project.org/package=colorfast) or
#'   the [farver][farver::farver] package.
#' @param x Color value to convert.
#' @return `col2int()` returns an integer.  `int2col()` returns a (hex) color string.
#' @examples
#' if (requireNamespace("farver", quietly = TRUE)) {
#'   int2col(col2int("red"))
#' }
#' @export
col2int <- function(x) {
    stopifnot(requireNamespace("colorfast", quietly = TRUE) ||
              requireNamespace("farver", quietly = TRUE))
    if (requireNamespace("colorfast", quietly = TRUE)) {
        colorfast::col_to_int(col2hex(x))
    } else {
        farver::encode_native(col2hex(x))
    }
}

as_native <- function(x) {
    if (is.numeric(x)) {
        as.integer(x)
    } else {
        col2int(col2hex(x))
    }
}

#' @rdname col2int
#' @importFrom utils packageVersion
#' @export
int2col <- function(x) {
    stopifnot(requireNamespace("farver", quietly = TRUE))
    col2hex(farver::decode_native(x))
}

`%||%` <- function(x, y) if (is.null(x)) y else x # nolint

# Technically we convert to Luma value using BT. 601 as recommended by
# https://poynton.ca/notes/colour_and_gamma/ColorFAQ.html
rgb2grey <- function(red, green, blue) {
    # 0.2126 * red + 0.7152 * green + 0.0722 * blue # BT. 709
    0.299 * red + 0.587 * green + 0.114 * blue # BT. 601
}

hex2grey <- function(col) {
    rgb2grey(hex2red(col), hex2green(col), hex2blue(col))
}

rgba2brightness <- function(red, green, blue, alpha = 1) {
    alpha * rgb2grey(red, green, blue)
}

hex2brightness <- function(col, alpha = NULL) {
    alpha <- alpha %||% hex2alpha(col)
    alpha * hex2grey(col)
}

rgba2darkness <- function(red, green, blue, alpha = 1) {
    alpha * (1 - rgb2grey(red, green, blue))
}

hex2darkness <- function(col, alpha = NULL) {
    alpha <- alpha %||% hex2alpha(col)
    alpha * (1 - hex2grey(col))
}

# From quick-and-dirty StackOverflow suggestion
mean_col <- function (x) {
    cols <- as.character(x)
    m <- grDevices::col2rgb(cols) / 255
    v <- apply(m, 1, quadratic_mean)
    grDevices::rgb(v[1L], v[2L], v[3L])
}
quadratic_mean <- function (x) sqrt(mean(x^2))

# From 0 to 1
hex2red <- function(col) {
    as.double(as.hexmode(substr(col, 2, 3))) / 255
}
# From 0 to 1
hex2green <- function(col) {
    as.double(as.hexmode(substr(col, 4, 5))) / 255
}
# From 0 to 1
hex2blue <- function(col) {
    as.double(as.hexmode(substr(col, 6, 7))) / 255
}
# From 0 to 1
hex2alpha <- function(col) {
    as.double(as.hexmode(substr(col, 8, 9))) / 255
}

# From 0 to 255
hex2alpha255 <- function(col) {
    as.integer(as.hexmode(substr(col, 8, 9)))
}
