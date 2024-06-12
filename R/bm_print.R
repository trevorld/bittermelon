#' Print bitmap objects
#'
#' `bm_print()` prints the bitmap object.
#' It is a wrapper around [as_bm_bitmap()] / [as_bm_pixmap()] and 
#' [print.bm_bitmap()] / [print.bm_pixmap()].
#'
#' @seealso [.S3method()] to register this as the print method for a target bitmap class.
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
#' @param ... Passed to [print.bm_pixmap()] or [print.bm_bitmap()] depending on the class of `x`.
#' @return A character vector of the string representation returned invisibly.
#'         As a side effect prints out the string representation to the terminal.
#' @export
bm_print <- function(x, ...) {
    if (inherits(x, c("bm_bitmap", "glyph_bitmap"))) {
        x <- as_bm_bitmap(x)
    } else {
        x <- as_bm_pixmap(x)
    }
    print(x, ...)
}
