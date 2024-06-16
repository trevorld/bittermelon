#' Print pixmap objects
#'
#' `print.bm_pixmap()` prints bittermelon pixmap objects to the terminal
#' It is a wrapper around `format.bm_pixmap()`.
#' @inheritSection bm_print Fonts and terminal settings
#' @param x A [bm_pixmap()] object
#' @param ... Currently ignored.
#' @param bg R color string of background color to use and/or
#'           cli ANSI style function of class `cli_ansi_style`.
#'           `FALSE` (default) for no background color
#'           (i.e. use default terminal background).
#' @param compress How to print the image:
#'          * "none" (default) or "n" use one character per pixel.
#'          * "vertical" or "v" use one character per two vertical pixels
#'            (makes pixels look closest to square in typical terminal).
#'          * "horizontal" or "h" use one character per two horizontal pixels.
#'          * "both" or "b" use one character per four pixels
#'            (this will be a lossy conversion whenever there are more than two colors per four pixels).
#' @param downscale If `TRUE` and the printed pixmap will be wider than `getOption("width")`
#'                  then shrink the image to fit `getOption("width")` using [bm_downscale()].
#' @return A character vector of the string representation (`print.bm_pixmap()` does this invisibly).
#'         As a side effect `print.bm_pixmap()` prints out the string representation to the terminal.
#' @examples
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn)
#' }
#'
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn, compress = "v", bg = cli::bg_br_white)
#' }
#'
#' if (cli::is_utf8_output() && 
#'     cli::num_ansi_colors() > 256L &&
#'     getOption("width") >= 100L) {
#'   img <- png::readPNG(system.file("img", "Rlogo.png", package="png"))
#'   pm <- as_bm_pixmap(img)
#'   print(pm, compress = "v")
#' }
#' @export
print.bm_pixmap <- function(x, ...,
                            bg = getOption("bittermelon.bg", FALSE),
                            compress = getOption("bittermelon.compress", "none"),
                            downscale = getOption("bittermelon.downscale", FALSE)) {
    # Avoid {cli} converting to ANSI style function if color string in `cli:::ansi_builtin_styles()`
    if (is.character(bg)) bg <- col2hex(bg)
    s <- format(x, ..., bg = bg, compress = compress, downscale = downscale)
    cat(s, sep = "\n")
    invisible(s)
}

#' @rdname print.bm_pixmap
#' @export
format.bm_pixmap <- function(x, ...,
                             bg = getOption("bittermelon.bg", FALSE),
                             compress = getOption("bittermelon.compress", "none"),
                             downscale = getOption("bittermelon.downscale", FALSE)) {
    if (nrow(x) == 0L || ncol(x) == 0L)
        return(character(0L))

    direction <- match.arg(tolower(compress),
                           c("none", "n", "vertical", "v", "horizontal", "h", "both", "b"))
    direction <- substr(direction, 1L, 1L)

    if (downscale) {
        x <- downscale_for_terminal(x, direction, filter = "Point")
    }

    if (!cli::is_utf8_output() || cli::num_ansi_colors() == 1L)
        return(format.bm_bitmap(as_bm_bitmap.bm_pixmap(x), bg = bg, compress = compress))

    x <- as.matrix(x)
    s <- switch(direction,
           n = format_bmr_none(x),
           h = format_bmr_horizontal(x),
           v = format_bmr_vertical(x),
           b = format_bmr_both(x),
           )
    if (!isFALSE(bg))
        s <- vapply(s, function(x) cli::make_ansi_style(bg, bg = TRUE)(x), character(1L))
    rev(s)
}

format_bmr_none <- function(x) {
    as.character(apply(x, 1L, row_bmr_none))
}

format_bmr_vertical <- function(x) {
    if(nrow(x) %% 2 > 0)
        x <- rbind(matrix("#FFFFFF00", ncol = ncol(x)), x)
    char <- apply(x, 2L, char_bmr_vertical)
    if (is.matrix(char))
        as.character(apply(char, 1L, function(x) paste(x, collapse = "")))
    else
        paste(char, collapse = "")
}

format_bmr_horizontal <- function(x) {
    if(ncol(x) %% 2 > 0)
        x <- cbind(x, matrix("#FFFFFF00", nrow = nrow(x)))
    as.character(apply(x, 1L, row_bmr_horizontal))
}

format_bmr_both <- function(x) {
    if(nrow(x) %% 2 > 0)
        x <- rbind(matrix("#FFFFFF00", ncol = ncol(x), byrow = TRUE), x)
    if(ncol(x) %% 2 > 0)
        x <- cbind(x, matrix("#FFFFFF00", nrow = nrow(x), byrow = TRUE))
    char <- matrix(" ", nrow = nrow(x) / 2, ncol = ncol(x) / 2)
    for (i in seq.int(2L, nrow(x), by = 2L)) {
        for (j in seq.int(2L, ncol(x), by = 2L)) {
            char[i/2, j/2] <- char_bmr_both(x[c(i-1, i), c(j-1, j)])
        }
    }
    as.character(apply(char, 1, function(x) paste(x, collapse = "")))
}

char_bmr_both <- function(x) {
    col <- c(x[2L, 1:2], x[1L, 1:2])
    transparent <- hex2alpha255(col) < 128L
    col <- substr(col, 1, 7)
    which_transparent <- which(transparent)
    if (length(which_transparent))
        col[which_transparent] <- "transparent"
    switch(length(unique(col)),
           char_bmr_both_1col(col, transparent),
           char_bmr_both_2col(col, transparent),
           char_bmr_both_3col(col, transparent),
           char_bmr_both_4col(col, transparent)
           )
}

char_bmr_both_1col <- function(col, transparent) {
    if (transparent[1])
        " "
    else
        fg2style(col[1L])("\u2588")
}

# up-right band "dominant"
char_diagonal <- function(col, transparent, col_rest, transparent_rest) {
    char_helper(col, transparent, col_rest, transparent_rest, "\u259a", "\u259e")
}

char_left <- function(col, transparent, col_rest, transparent_rest) {
    char_helper(col, transparent, col_rest, transparent_rest, "\u258c", "\u2590")
}

char_top <- function(col, transparent, col_rest, transparent_rest) {
    char_helper(col, transparent, col_rest, transparent_rest, "\u2580", "\u2584")
}

char_upper_left <- function(col, transparent, col_rest, transparent_rest) {
    char_helper(col_rest, transparent_rest, col, transparent, "\u259f", "\u2598")
}

char_upper_right <- function(col, transparent, col_rest, transparent_rest) {
    char_helper(col_rest, transparent_rest, col, transparent, "\u2599", "\u259d")
}

char_bottom_left <- function(col, transparent, col_rest, transparent_rest) {
    char_helper(col_rest, transparent_rest, col, transparent, "\u259c", "\u2596")
}

char_bottom_right <- function(col, transparent, col_rest, transparent_rest) {
    char_helper(col_rest, transparent_rest, col, transparent, "\u259b", "\u2597")
}

char_helper <- function(col, transparent, col_rest, transparent_rest, char, char_rest) {
    if (transparent) {
        fg2style(col_rest)(char_rest)
    } else if (transparent_rest) {
        fg2style(col)(char)
    } else {
        cli::combine_ansi_styles(
                                 fg2style(col),
                                 fg2style(col_rest, bg = TRUE)
                                 )(char)
    }
}

# order (2,1) (2,2) (1,1) (1,2)
char_bmr_both_2col <- function(col, transparent) {
    if (col[1L] == col[3L] && col[2L] == col[4L]) { # 2 vertical bands
        char_left(col[1L], transparent[1L], col[2L], transparent[2L])
    } else if (col[1L] == col[2L] && col[3L] == col[4L]) { # 2 horizontal bands
        char_top(col[1L], transparent[1L], col[3L], transparent[3L])
    } else if (col[1L] == col[4L] && col[2L] == col[3L]) { # 2 diagonal bands
        char_diagonal(col[1L], transparent[1L], col[2L], transparent[2L]) # up-right band "dominant"
    } else if (col[2L] == col[3L] && col[2L] == col[4L]) { # just upper left col[1L]
        char_upper_left(col[1L], transparent[1L], col[2L], transparent[2L])
    } else if (col[1L] == col[3L] && col[1L] == col[4L]) { # just upper right col[2L]
        char_upper_right(col[2L], transparent[2L], col[1L], transparent[1L])
    } else if (col[1L] == col[2L] && col[1L] == col[4L]) { # just bot. left col[3L]
        char_bottom_left(col[3L], transparent[3L], col[1L], transparent[1L])
    } else { # just bot. right col[4]
        char_bottom_right(col[4L], transparent[4L], col[1L], transparent[1L])
    }
}

# ToDo: Better 3-color approximation
char_bmr_both_3col <- function(col, transparent) {
    if (any(transparent)) {
        col[!transparent] <- mean_col(col[!transparent])
        char_bmr_both_2col(col, transparent)
    } else {
        char_top(mean_col(col[1:2]), FALSE, mean_col(col[3:4]), FALSE)
    }
}
# ToDo: Better 4-color approximation
char_bmr_both_4col <- function(col, transparent) {
    if (any(transparent)) {
        col[!transparent] <- mean_col(col[!transparent])
        char_bmr_both_2col(col, transparent)
    } else {
        char_top(mean_col(col[1:2]), FALSE, mean_col(col[3:4]), FALSE)
    }
}

row_bmr_horizontal <- function(col) {
    alpha <- hex2alpha255(col)
    fg <- substr(col, 1, 7)
    opaque <- as.logical(.bincode(alpha, breaks = c(-1, 127, 255)) - 1L)
    transparent <- !opaque
    char <- character(length = length(col) / 2)
    for (i in seq.int(1L, length(col), by = 2L)) {
        j <- i / 2 + 1
        if (opaque[i] && opaque[i+1L] && fg[i] == fg[i+1L]) {
            char[j] <- fg2style(fg[i])("\u2588")
        } else if (transparent[i] && transparent[i+1L]) {
            char[j] <- " "
        } else {
            char[j] <- char_left(fg[i], transparent[i], fg[i+1L], transparent[i+1L])
        }
    }
    paste(char, collapse = "")
}

char_bmr_vertical <- function(col) {
    alpha <- as.integer(as.hexmode(substr(col, 8, 9)))
    fg <- substr(col, 1, 7)
    opaque <- as.logical(.bincode(alpha, breaks = c(-1, 127, 255)) - 1L)
    transparent <- !opaque
    char <- character(length = length(col) / 2)
    for (i in seq.int(1L, length(col), by = 2L)) {
        j <- i / 2 + 1
        if (opaque[i] && opaque[i+1L] && fg[i] == fg[i+1L]) {
                char[j] <- fg2style(fg[i])("\u2588")
        } else if (transparent[i] && transparent[i+1L]) {
            char[j] <- " "
        } else {
            char[j] <- char_top(fg[i+1], transparent[i+1], fg[i], transparent[i])
        }
    }
    char
}

#### Palette substitution
fg2style <- function(col, bg = FALSE) {
    cli::make_ansi_style(col, bg = bg)
}

glyph_bmr_none <- function(col) {
    alpha <- as.integer(as.hexmode(substr(col, 8, 9)))
    bin <- .bincode(alpha, breaks = c(-1, 31, 95, 159, 223, 255))
    c(" ", "\u2591", "\u2592", "\u2593", "\u2588")[bin]
}

fg_bmr_none <- function(col) {
    substr(col, 1, 7)
}

row_bmr_none <- function(col) {
    glyphs <- glyph_bmr_none(col)
    fg <- fg_bmr_none(col)
    s <- .mapply(function(glyph, fg) {
                     if (glyph == " ")
                         " "
                     else
                         fg2style(fg)(glyph)
                },
                dots = list(glyph=glyphs, fg=fg),
                MoreArgs = list()
                )
    paste(s, collapse = "")
}
