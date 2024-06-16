#' Print bitmap objects
#'
#' `bm_print()` prints a representation of the bitmap object to the terminal while
#' `bm_format()` returns just the character vector without printing it.
#' They are wrappers around [as_bm_bitmap()] / [as_bm_pixmap()] and 
#' [format.bm_bitmap()] / [format.bm_pixmap()].
#' @details # Fonts and terminal settings
#' Printing bitmaps/pixmaps may or may not look great 
#' in your terminal depending on a variety of factors:
#'
#' * The terminal should support the Unicode - UTF-8 encoding.  
#'   We use [cli::is_utf8_output()] to guess Unicode support
#'   which in turn looks at `getOption("cli.unicode")` and [l10n_info()].
#'
#' * The terminal should support ANSI sequences and if it does
#'   it should support many colors.
#'
#'   + We use [cli::num_ansi_colors()] to detect number of colors supported.
#'     `num_ansi_colors()` detection algorithm is complicated but it first looks at
#'     `getOption("cli.num_colors")`.
#'   * If `cli::num_ansi_colors()` equals `r cli::truecolor` then your terminal
#'     supports 24-bit ANSI colors.
#'   + If using the Windows Command Prompt window you may need to enable
#'     ANSI sequences support by doing `REG ADD HKCU\CONSOLE /f /v VirtualTerminalLevel /t REG_DWORD /d 1`
#'     from the command-line or running `regedit` (Registry Editor) and go to
#'     `Computer\HKEY_CURRENT_USER\Console` and set `VirtualTerminalLevel` to `1`.
#'
#' * The font used by the terminal should be a monoscale font that supports the 
#'   [Block Elements](https://en.wikipedia.org/wiki/Block_Elements) Unicode block.
#'
#' * The terminal text settings should have a cell spacing around 1.00 times width and 1.00 times height.
#'   For terminals configured by CSS styles this means a `line-height` of around `1.0`.
#'
#' @seealso [.S3method()] to register this as the print method for a non-bittermelon bitmap class.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- as_bm_bitmap("R", font = font)
#' bm_print(capital_r)
#'
#' corn_r <- as.raster(farming_crops_16x16()$corn$portrait)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   bm_print(corn_r, compress = "v") 
#' }
#'
#' if (requireNamespace("magick", quietly = TRUE) &&
#'     cli::is_utf8_output() && 
#'     cli::num_ansi_colors() > 256L) {
#'   rose_mi <- magick::image_read("rose:")
#'   bm_print(rose_mi, compress = "v") 
#' }
#'
#' \dontrun{# Change other bitmap classes' `print()` to use `bm_print()` instead
#'   options(bittermelon.compress = "vertical",
#'           bittermelon.downscale = requireNamespace("magick", quietly = TRUE))
#'   for (cl in c("glyph_bitmap", "magick-image", "nativeRaster", "pixeltrix",
#'                "pixmapGrey", "pixmapIndexed", "pixmapRGB", "raster")) {
#'     .S3method("print", cl, bittermelon::bm_print)
#'   }
#' }
#' @param x A bitmap object that can be cast by [as_bm_pixmap()] to a [bm_pixmap()] object.
#' @param ... Passed to [format.bm_pixmap()] or [format.bm_bitmap()] depending on the class of `x`.
#' @return A character vector of the string representation (`bm_print()` returns this invisibly).
#'         As a side effect `bm_print()` prints out the string representation to the terminal.
#' @export
bm_print <- function(x, ...) {
    s <- bm_format(x, ...)
    cat(s, sep = "\n")
    invisible(s)
}

#' @rdname bm_print
#' @export
bm_format <- function(x, ...) {
    if (inherits(x, c("bm_bitmap", "glyph_bitmap"))) {
        x <- as_bm_bitmap(x)
    } else {
        x <- as_bm_pixmap(x)
    }
    format(x, ...)
}
