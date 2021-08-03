#' Combine bitmaps by rows or columns
#'
#' `cbind.bm_bitmap()` and `rbind.bm_bitmap()` combine
#' by columns or rows respectively.
#' @param ... [bm_bitmap()] objects.
#' @param direction For `cbind().bm_bitmap` either "left-to-right" (default) or its alias "lr"
#'                  OR "right-to-left" or its alias "rl".
#'                  For `rbind().bm_bitmap` either "top-to-bottom" (default) or its alias "tb"
#'                  OR "bottom-to-top" or its alias "bt".
#' @return A [bm_bitmap()] object.
#' @rdname bm_bind
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   capital_b <- font[[str2ucp("B")]]
#'   capital_m <- font[[str2ucp("M")]]
#'   cbm <- cbind(capital_b, capital_m)
#'   print(cbm, px = c(".", "#"))
#'   cbm_rl <- cbind(capital_b, capital_m, direction = "right-to-left")
#'   print(cbm_rl, px = c(".", "#"))
#'   rbm <- rbind(capital_b, capital_m)
#'   print(rbm, px = c(".", "#"))
#'   rbm_bt <- rbind(capital_b, capital_m, direction = "bottom-to-top")
#'   print(rbm_bt, px = c(".", "#"))
#' @export
cbind.bm_bitmap <- function(..., direction = "left-to-right") {
    direction <- match.arg(direction, c("left-to-right", "lr", "right-to-left", "rl"))
    l <- lapply(list(...), as.matrix)
    if (direction %in% c("right-to-left", "rl"))
        l <- rev(l)
    bm_bitmap(do.call(cbind, l))
}

#' @rdname bm_bind
#' @export
rbind.bm_bitmap <- function(..., direction = "top-to-bottom") {
    direction <- match.arg(direction, c("top-to-bottom", "tb", "bottom-to-top", "bt"))
    l <- rev(lapply(list(...), as.matrix))
    if (direction %in% c("bottom-to-top", "bt"))
        l <- rev(l)
    bm_bitmap(do.call(rbind, l))
}
