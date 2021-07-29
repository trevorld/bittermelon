#' Read hex format bitmap font files
#'
#' `read_hex()` reads in hex format bitmap font files
#' as a [bm_font()] object.
#' @param con A connection object or a character string.
#'            This can be a compressed file.
#'            See [base::readLines()] for more info.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[code_point_from_name("LATIN CAPITAL LETTER R")]]
#'  print(capital_r, labels = c(".", "#"))
#' @export
#' @seealso [bm_font()]
read_hex <- function(con) {
    contents <- readLines(con)
    contents <- grep("^[0-9]+:[A-Fa-f0-9]+$", contents, value = TRUE)
    contents <- strsplit(contents, ":")

    code_points <- sapply(contents, function(x) x[1])
    code_points <- code_point(code_points)

    glyphs <- lapply(contents, function(x) as_bm_glyph_hex(x[2]))

    names(glyphs) <- code_points
    bm_font(glyphs)
}

#' Write hex format bitmap font files
#'
#' `write_hex()` writes hex format bitmap font files
#' @param font A [bm_font()] object.
#' @param con A connection object or a character string.
#'            See [base::writeLines()] for more info.
#' @noRd
write_hex <- function(font, con) {
    NULL # To be completed
}

as_bm_glyph_hex <- function(hex_string) {
    n_hex <- nchar(hex_string)
    stopifnot(n_hex == 32L || n_hex == 64L)
    if (n_hex == 32L) {
        n_i <- 2L
        m <- matrix(0L, nrow = 16L, ncol = 8L)
    } else if (n_hex == 64L) {
        n_i <- 4L
        m <- matrix(0L, nrow = 16L, ncol = 16L)
    }
    hex_string <- tolower(hex_string)
    for (i in seq.int(16)) {
        stop <- n_hex - (i - 1L) * n_i
        start <- stop - n_i + 1L
        hex_substring <- substr(hex_string, start, stop)
        hexes <- strsplit(hex_substring, "")[[1]]
        m[i, ] <- unlist(lapply(hexes, hex_to_binary))
    }
    bm_glyph(m)
}

hex_to_binary <- function(hex) {
    switch(hex,
           "0" = c(0L, 0L, 0L, 0L),
           "1" = c(0L, 0L, 0L, 1L),
           "2" = c(0L, 0L, 1L, 0L),
           "3" = c(0L, 0L, 1L, 1L),
           "4" = c(0L, 1L, 0L, 0L),
           "5" = c(0L, 1L, 0L, 1L),
           "6" = c(0L, 1L, 1L, 0L),
           "7" = c(0L, 1L, 1L, 1L),
           "8" = c(1L, 0L, 0L, 0L),
           "9" = c(1L, 0L, 0L, 1L),
           "a" = c(1L, 0L, 1L, 0L),
           "b" = c(1L, 0L, 1L, 1L),
           "c" = c(1L, 1L, 0L, 0L),
           "d" = c(1L, 1L, 0L, 1L),
           "e" = c(1L, 1L, 1L, 0L),
           "f" = c(1L, 1L, 1L, 1L))
}

binary_to_hex <- function(hex) {
    switch(hex,
           "0000" = "0",
           "0001" = "1",
           "0010" = "2",
           "0011" = "3",
           "0100" = "4",
           "0101" = "5",
           "0110" = "6",
           "0111" = "7",
           "1000" = "8",
           "1001" = "9",
           "1010" = "A",
           "1011" = "B",
           "1100" = "C",
           "1101" = "D",
           "1110" = "E",
           "1111" = "F")
}
