#' Print bitmap glyph objects
#'
#' `print.bm_glyph()` prints a representation of bitmap glyph objects to the terminal
#'
#' @examples
#'   plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'   plus_sign[5L, 3:7] <- 1L
#'   plus_sign[3:7, 5L] <- 1L
#'   plus_sign <- bm_glyph(plus_sign)
#'   print(plus_sign, labels = c(" ", "#"))
#'
#'   space_glyph <- bm_glyph(matrix(0L, nrow = 8L, ncol = 8L))
#'   print(space_glyph, labels = ".")
#' @seealso [bm_glyph()]
#' @inheritParams as.character.bm_glyph
#' @return A character vector of the string representation invisibly.
#' @usage \method{print}{bm_glyph}(x, ..., labels = c("\u2591", "\u2588", "\u2592"))
#' @export
print.bm_glyph <- function(x, ..., labels = c("\u2591", "\u2588", "\u2592")) {
    x <- as_bm_glyph(x)
    cat(as.character(x, ..., labels = labels), sep = "\n")
    invisible(x)
}
