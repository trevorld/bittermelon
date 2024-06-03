#' Resize images via distortion.
#'
#' `bm_distort()` resize images to arbitrary width and height value via distortion.
#'
#' `bm_distort()` generates a distorted [grid::rasterGrob()] with the help of
#' [as.raster.bm_bitmap()] which is then converted back to a
#' [bm_bitmap()] via [as_bm_bitmap.grob()].
#'
#' @inheritParams as.raster.bm_bitmap
#' @inheritParams as_bm_bitmap.grob
#' @inheritParams bm_clamp
#' @param vp A [grid::viewport()] object that could be used to further manipulate the image.
#' @inherit bm_clamp return
#'
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  dim(capital_r) # 8 x 16
#'  if (require("grid") && capabilities("png")) {
#'    print(bm_distort(capital_r, width = 9L, height = 21L))
#'  }
#' @seealso [bm_expand()] for expanding width/height by integer multiples.
#'          [bm_resize()] for resizing an image via trimming/extending an image.
#' @export
bm_distort <- function(x, width = NULL, height = NULL, ...) {
    UseMethod("bm_distort")
}

#' @rdname bm_distort
#' @export
bm_distort.bm_bitmap <- function(x, width = NULL, height = NULL,
                                 interpolate = FALSE, vp = NULL,
                                 png_device = NULL, threshold = 0.25) {
    bm_distort_bitmap(x, 
                      width = width, height = height,
                      interpolate = interpolate, vp = vp,
                      png_device = png_device, threshold = threshold)
}

#' @rdname bm_distort
#' @export
bm_distort.bm_list <- function(x, width = NULL, height = NULL,
                               interpolate = FALSE, vp = NULL,
                               png_device = NULL, threshold = 0.25) {
    bm_lapply(x, bm_distort_bitmap,
              width = width, height = height,
              interpolate = interpolate, vp = vp,
              png_device = png_device, threshold = threshold)
}

bm_distort_bitmap <- function(x, width = NULL, height = NULL,
                              interpolate = FALSE, vp = NULL,
                              png_device = NULL, threshold = 0.25) {
    if (is.null(width))
        width <- ncol(x)
    if (is.null(height))
        height <- nrow(x)
    col <- c("transparent", rep_len(1L, max(max(x) - 1L, 1L)))
    r <- as.raster(x, col = col)
    grob <- grid::rasterGrob(r, interpolate = interpolate, width = 1, height = 1, vp = vp)
    as_bm_bitmap.grob(grob, width = width, height = height,
                      png_device = png_device, threshold = threshold)
}
