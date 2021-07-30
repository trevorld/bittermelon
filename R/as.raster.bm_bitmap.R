#' Coerce bitmap glyph object to raster object
#'
#' `as.raster.bm_bitmap()` converts a bitmap glyph object to a raster object.
#'
#' @inheritParams as.character.bm_bitmap
#' @param col Character vector of R color specifications
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   capital_r <- font[[str2ucp("R")]]
#'   grDevices::as.raster(capital_r)
#' @importFrom grDevices as.raster
#' @export
as.raster.bm_bitmap <- function(x, ..., col = c("grey80", "black", "grey40")) { # nolint
    x <- as.matrix(as_bm_bitmap(x))
    x <- x[rev(seq_len(nrow(x))), ]
    r <- apply(x, 2, function(i) col[i + 1L])
    grDevices::as.raster(r)
}
