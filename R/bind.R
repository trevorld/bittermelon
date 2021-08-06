#' Combine bitmaps by rows or columns
#'
#' `cbind.bm_bitmap()` and `rbind.bm_bitmap()` combine
#' by columns or rows respectively.
#' @param ... [bm_bitmap()] objects.
#' @param direction For `cbind().bm_bitmap` either "left-to-right" (default) or its aliases "ltr" and "lr"
#'                  OR "right-to-left" or its aliases "rtl" and "rl".
#'                  For `rbind().bm_bitmap` either "top-to-bottom" (default) or its aliases "ttb" and "tb"
#'                  OR "bottom-to-top" or its aliases "btt" and "bt".
#'                  The `direction` argument is not case-sensitive.
#' @param vjust Used by [bm_extend()] if bitmap heights are different.
#' @param hjust Used by [bm_extend()] if bitmap widths are different.
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
cbind.bm_bitmap <- function(..., direction = "left-to-right", vjust = "center-top") {
    is_ltr <- c(tolower(direction) %in% c("left-to-right", "ltr", "lr"))
    is_rtl <- c(tolower(direction) %in% c("right-to-left", "rtl", "rl"))
    stopifnot(is_ltr || is_rtl)
    bml <- bm_list(...)
    heights <- bm_heights(bml)
    if (length(heights) > 1L)
        bml <- bm_extend(bml, height = max(heights), vjust = vjust)
    l <- lapply(bml, as.matrix)
    if (is_rtl)
        l <- rev(l)
    bm_bitmap(do.call(cbind, l))
}

#' @rdname bm_bind
#' @export
rbind.bm_bitmap <- function(..., direction = "top-to-bottom", hjust = "center-left") {
    is_ttb <- c(tolower(direction) %in% c("top-to-bottom", "ttb", "tb"))
    is_bbt <- c(tolower(direction) %in% c("bottom-to-top", "bbt", "bt"))
    stopifnot(is_ttb || is_bbt)
    bml <- bm_list(...)
    widths <- bm_widths(bml)
    if (length(widths) > 1L)
        bml <- bm_extend(bml, width = max(widths), hjust = hjust)
    l <- lapply(bml, as.matrix)
    if (is_ttb)
        l <- rev(l)
    bm_bitmap(do.call(rbind, l))
}
