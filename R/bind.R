#' Combine bitmaps by rows or columns
#'
#' `cbind.bm_bitmap()` and `rbind.bm_bitmap()` combine
#' by columns or rows respectively.
#' @param ... [bm_bitmap()] objects.
#' @return A [bm_bitmap()] object.
#' @rdname bm_bind
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   capital_b <- font[[str2ucp("B")]]
#'   capital_m <- font[[str2ucp("M")]]
#'   cbm <- cbind(capital_b, capital_m)
#'   print(cbm, labels = c(".", "#"))
#'   rbm <- rbind(capital_b, capital_m)
#'   print(rbm, labels = c(".", "#"))
#' @export
cbind.bm_bitmap <- function(...) {
    l <- lapply(list(...), as.matrix)
    bm_bitmap(do.call(cbind, l))
}

#' @rdname bm_bind
#' @export
rbind.bm_bitmap <- function(...) {
    l <- rev(lapply(list(...), as.matrix))
    bm_bitmap(do.call(rbind, l))
}
