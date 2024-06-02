#' Combine bitmap/pixmap objects by rows or columns
#'
#' `cbind.bm_bitmap()` / `cbind.bm_pixmap()` and
#' `rbind.bm_bitmap()` / `rbind.bm_pixmap()`
#' combine by columns or rows respectively.
#'
#' @param ... [bm_bitmap()] or [bm_pixmap()] objects.
#' @param direction For `cbind()` either "left-to-right" (default) or its aliases "ltr" and "lr"
#'                  OR "right-to-left" or its aliases "rtl" and "rl".
#'                  For `rbind()` either "top-to-bottom" (default) or its aliases "ttb" and "tb"
#'                  OR "bottom-to-top" or its aliases "btt" and "bt".
#'                  The `direction` argument is not case-sensitive.
#' @param vjust Used by [bm_extend()] if bitmap heights are different.
#' @param hjust Used by [bm_extend()] if bitmap widths are different.
#' @return A [bm_bitmap()] or [bm_pixmap()] object.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   capital_b <- font[[str2ucp("B")]]
#'   capital_m <- font[[str2ucp("M")]]
#'   cbm <- cbind(capital_b, capital_m)
#'   print(cbm)
#'   cbm_rl <- cbind(capital_b, capital_m, direction = "right-to-left")
#'   print(cbm_rl)
#'   rbm <- rbind(capital_b, capital_m)
#'   print(rbm)
#'   rbm_bt <- rbind(capital_b, capital_m, direction = "bottom-to-top")
#'   print(rbm_bt)
#' @rdname bm_bind
#' @export
cbind.bm_bitmap <- function(..., direction = "left-to-right", vjust = "center-top") {
    is_ltr <- c(tolower(direction) %in% c("left-to-right", "ltr", "lr"))
    is_rtl <- c(tolower(direction) %in% c("right-to-left", "rtl", "rl"))
    stopifnot(is_ltr || is_rtl)
    bml <- list(...)
    bml <- lapply(bml, as_bm_bitmap)
    heights <- unique(vapply(bml, nrow, integer(1L)))
    if (length(heights) > 1L)
        bml <- lapply(bml, bm_extend, height = max(heights), vjust = vjust)
    l <- lapply(bml, as.matrix)
    if (is_rtl)
        l <- rev(l)
    as_bm_bitmap.matrix(do.call(cbind, l))
}

#' @rdname bm_bind
#' @export
rbind.bm_bitmap <- function(..., direction = "top-to-bottom", hjust = "center-left") {
    is_ttb <- c(tolower(direction) %in% c("top-to-bottom", "ttb", "tb"))
    is_bbt <- c(tolower(direction) %in% c("bottom-to-top", "bbt", "bt"))
    stopifnot(is_ttb || is_bbt)
    bml <- list(...)
    bml <- lapply(bml, as_bm_bitmap)
    widths <- unique(vapply(bml, ncol, integer(1L)))
    if (length(widths) > 1L)
        bml <- lapply(bml, bm_extend, width = max(widths), hjust = hjust)
    l <- lapply(bml, as.matrix)
    if (is_ttb)
        l <- rev(l)
    as_bm_bitmap.matrix(do.call(rbind, l))
}

#' @rdname bm_bind
#' @export
cbind.bm_pixmap <- function(..., direction = "left-to-right", vjust = "center-top") {
    is_ltr <- c(tolower(direction) %in% c("left-to-right", "ltr", "lr"))
    is_rtl <- c(tolower(direction) %in% c("right-to-left", "rtl", "rl"))
    stopifnot(is_ltr || is_rtl)
    bml <- list(...)
    bml <- lapply(bml, as_bm_pixmap)
    heights <- unique(vapply(bml, nrow, integer(1L)))
    if (length(heights) > 1L)
        bml <- lapply(bml, bm_extend, height = max(heights), vjust = vjust)
    l <- lapply(bml, as.matrix)
    if (is_rtl)
        l <- rev(l)
    as_bm_pixmap.matrix(do.call(cbind, l))
}

#' @rdname bm_bind
#' @export
rbind.bm_pixmap <- function(..., direction = "top-to-bottom", hjust = "center-left") {
    is_ttb <- c(tolower(direction) %in% c("top-to-bottom", "ttb", "tb"))
    is_bbt <- c(tolower(direction) %in% c("bottom-to-top", "bbt", "bt"))
    stopifnot(is_ttb || is_bbt)
    bml <- list(...)
    bml <- lapply(bml, as_bm_pixmap)
    widths <- unique(vapply(bml, ncol, integer(1L)))
    if (length(widths) > 1L)
        bml <- lapply(bml, bm_extend, width = max(widths), hjust = hjust)
    l <- lapply(bml, as.matrix)
    if (is_ttb)
        l <- rev(l)
    as_bm_pixmap.matrix(do.call(rbind, l))
}
