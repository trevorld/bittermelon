#' Read and write hex bitmap font files
#'
#' `read_hex()` reads in hex format bitmap font files
#' as a [bm_font()] object while `write_hex()` writes a [bm_font()] object
#' as a hex format bitmap font file.
#' @param con A connection object or a character string of a filename.
#'            See [base::readLines()] or [base::writeLines()] for more info.
#'            If it is a connection it will be explicitly closed.
#' @param ucp Character vector of Unicode Code Points: glyphs not in this vector won't be read in.
#'            If `NULL` (default) read every glyph in the font.
#' @param font A [bm_font()] object.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = px_ascii)
#'
#'  filename <- tempfile(fileext = ".hex.gz")
#'  write_hex(font, gzfile(filename))
#'
#'  font <- read_hex(font_file, ucp = block2ucp("Basic Latin"))
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = px_ascii)
#' @export
#' @rdname hex_font
#' @return `read_hex()` returns a [bm_font()] object.
#'         `write_hex()` returns invisibly a character vector of the contents
#'         of the hex font file it wrote to `con` as a side effect.
#' @seealso [bm_font()]
read_hex <- function(con, ucp = NULL) {
    stopifnot(is.null(ucp) || all(is_ucp(ucp)))
    if (inherits(con, "connection"))
        on.exit(close(con))

    contents <- readLines(con)

    comments <- capture_comments(contents)

    glyphs <- grep("^[A-Fa-f0-9]+:[A-Fa-f0-9]+$", contents, value = TRUE)
    if (length(glyphs) == 0L) {
        glyphs <- bm_list()
    } else {
        glyphs <- strsplit(glyphs, ":")

        code_points <- sapply(glyphs, function(x) x[1])
        code_points <- hex2ucp(code_points)

        if (is.null(ucp)) {
            glyphs <- lapply(glyphs, function(x) as_bm_bitmap_hex(x[2]))
            names(glyphs) <- code_points
        } else {
            indices <- which(code_points %in% ucp)
            glyphs <- lapply(glyphs[indices], function(x) as_bm_bitmap_hex(x[2]))
            names(glyphs) <- code_points[indices]
        }
    }

    bm_font(glyphs, comments = comments)
}

#' @rdname hex_font
#' @export
write_hex <- function(font, con = stdout()) {
    if (inherits(con, "connection"))
        on.exit(close(con))

    validate_bm_font(font)

    # hex fonts only support black-and-white glyphs
    if (any(sapply(font, function(x) max(x) > 1L))) {
        message("Multi-colored glyphs detected, casting to black-and-white.")
        font <- bm_clamp(font)
    }

    contents <- c()

    comments <- attr(font, "comments")
    if (length(comments)) {
        comments <- paste0("# ", comments)
        contents <- c(contents, comments)
    }

    # hex fonts only support 8x16 and 16x16 glyphs
    heights <- sapply(font, nrow)
    stopifnot(all(heights == 16L))
    widths <- sapply(font, ncol)
    stopifnot(all(widths == 8L | widths == 16L))
    code_points <- hex2ucp(names(font))
    code_points <- substr(code_points, 3L, nchar(code_points))

    glyphs <- sapply(font, as_hex)
    hex <- paste0(code_points, ":", glyphs)
    contents <- c(contents, hex)

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
            binary <- paste(format(glyph[i, j_indices]), collapse = "")
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
