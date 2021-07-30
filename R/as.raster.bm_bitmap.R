#' Coerce bitmap glyph object to raster object
#'
#' `as.raster.bm_bitmap()` converts a bitmap glyph object to a raster object.
#'
#' @inheritParams as.character.bm_bitmap
#' @param col Character vector of R color specifications
#' @examples
#'   plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'   plus_sign[5L, 3:7] <- 1L
#'   plus_sign[3:7, 5L] <- 1L
#'   plus_sign <- bm_bitmap(plus_sign)
#'   grDevices::as.raster(plus_sign)
#' @importFrom grDevices as.raster
#' @export
as.raster.bm_bitmap <- function(x, ..., col = c("grey80", "black", "grey40")) { # nolint
    x <- as.matrix(as_bm_bitmap(x))
    x <- x[rev(seq_len(nrow(x))), ]
    r <- apply(x, 2, function(i) col[i + 1L])
    grDevices::as.raster(r)
}
