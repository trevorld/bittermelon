#' Merge bitmaps by overlaying one over another
#'
#' `bm_overlay()` merges bitmaps by overlaying a bitmap over another.
#'
#' If necessary bitmaps will be extended by `bm_extend()` such that
#' they are the same size.  Then the non-zero pixels of the \dQuote{over}
#' bitmap will be inserted into the \dQuote{under} bitmap.
#'
#' @inheritParams bm_extend
#' @param over A 'bm_bitmap()' object to overlay
#'             over the `x` bitmap(s).
#'             Only one of `over` or `under` may be set.
#' @param under A 'bm_bitmap()' object which will be overlaid
#'             by the `x` bitmap(s).
#'             Only one of `over` or `under` may be set.
#' @inherit bm_clamp return
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   grave <- font[[str2ucp("`")]]
#'   a <- font[[str2ucp("a")]]
#'   a_grave <- bm_overlay(a, over = grave)
#'   print(a_grave)
#'
#'   # Can also instead specify the under glyph as a named argument
#'   a_grave2 <- bm_overlay(grave, under = a)
#'   print(a_grave2)
#' @export
bm_overlay <- function(x, over = NULL, under = NULL,
                       hjust = "center-left", vjust = "center-top") {
    stopifnot(is.null(over) || is.null(under))

    modify_bm_bitmaps(x, bm_overlay_bitmap,
                      over = over, under = under,
                      hjust = hjust, vjust = vjust)
}

bm_overlay_bitmap <- function(x, over = NULL, under = NULL,
                              hjust = "center-left", vjust = "center-top") {
    if (is.null(over)) {
        over <- x
        stopifnot(is_bm_bitmap(under))
    } else {
        under <- x
        stopifnot(is_bm_bitmap(over))
    }
    if (ncol(over) > ncol(under))
        under <- bm_extend(under, width = ncol(over), hjust = hjust)
    if (ncol(under) > ncol(over))
        over <- bm_extend(over, width = ncol(under), hjust = hjust)
    if (nrow(over) > nrow(under))
        under <- bm_extend(under, height = nrow(over), vjust = vjust)
    if (nrow(under) > nrow(over))
        over <- bm_extend(over, height = nrow(under), vjust = vjust)
    indices <- which(as.logical(over > 0L))
    under[indices] <- over[indices]
    under
}
