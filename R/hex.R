#' Read and write hex bitmap font files
#'
#' `read_hex()` reads in hex format bitmap font files
#' as a [bm_font()] object while `write_hex()` writes a [bm_font()] object
#' as a hex format bitmap font file.
#' @param con A connection object or a character string of a filename.
#'            See [base::readLines()] or [base::writeLines()] for more info.
#'            If it is a connection it will be explicitly closed.
#'
#' @param font A [bm_font()] object.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, labels = c(".", "#"))
#'
#'  filename <- tempfile(fileext = ".hex.gz")
#'  write_hex(font, gzfile(filename))
#' @export
#' @rdname hex_font
#' @seealso [bm_font()]
read_hex <- function(con) {
    if (inherits(con, "connection"))
        on.exit(close(con))

    contents <- readLines(con)

    contents <- grep("^[A-Fa-f0-9]+:[A-Fa-f0-9]+$", contents, value = TRUE)
    contents <- strsplit(contents, ":")

    code_points <- sapply(contents, function(x) x[1])
    code_points <- hex2ucp(code_points)

    glyphs <- lapply(contents, function(x) as_bm_bitmap_hex(x[2]))

    names(glyphs) <- code_points
    bm_font(glyphs)
}

#' @rdname hex_font
#' @export
write_hex <- function(font, con = stdout()) {
    if (inherits(con, "connection"))
        on.exit(close(con))

    validate_bm_font(font)
    # hex fonts only support 8x16 and 16x16 glyphs
    heights <- sapply(font, nrow)
    stopifnot(all(heights == 16L))
    widths <- sapply(font, ncol)
    stopifnot(all(widths == 8L | widths == 16L))
    code_points <- hex2ucp(names(font))
    code_points <- substr(code_points, 3L, nchar(code_points))

    if (any(sapply(font, function(x) max(x) > 1L))) {
        message("Multi-colored glyphs detected, casting to black-and-white.")
        font <- bm_clamp(font)
    }
    glyphs <- sapply(font, as_hex)

    hex <- paste0(code_points, ":", glyphs)

    writeLines(hex, con)
    invisible(hex)
}

as_bm_bitmap_hex <- function(hex_string) {
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
    bm_bitmap(m)
}

as_hex <- function(glyph) {
    val <- ""
    # we're going to work from bottom-to-top, right-to-left
    for (i in seq_len(nrow(glyph))) {
        for (j in rev(seq_len(ncol(glyph) / 4L))) {
            j_indices <- seq.int(4L * (j - 1L) + 1L, length.out = 4L)
            binary <- paste(as.character(glyph[i, j_indices]), collapse = "")
            hex <- binary_to_hex(binary)
            val <- paste0(hex, val)
        }
    }
    val
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
