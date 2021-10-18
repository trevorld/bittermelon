#' Compose graphemes in a bitmap list by applying combining marks
#'
#' `bm_compose()` simplifies `bm_list()` object
#' by applying combining marks to preceding glpyhs (composing new graphemes).
#'
#' `bm_compose()` identifies combining marks by their name using [is_combining_character()].
#' It then combines such marks with their immediately preceding glyph using [bm_overlay()].
#'
#' @param bml A `bm_list()` object.
#'            All combining marks need appropriate Unicode code point names
#'            to be recognized by [is_combining_character()].
#' @inheritParams is_combining_character
#' @param ... Passed to [bm_overlay()].
#' @return A `bm_list()` object.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   grave <- font[[str2ucp("`")]]
#'   a <- font[[str2ucp("a")]]
#'   bml <- bm_list(`U+0061` = a, `U+0300` = grave)
#'   print(bml, px = px_ascii)
#'   print(bm_compose(bml), px = px_ascii)
#' @export
bm_compose <- function(bml, pua_combining = character(0), ...) {
    if (length(bml) < 2L) return(bml)

    n_orig <- length(bml)
    i_combining <- is_combining_character(names(bml), pua_combining = pua_combining)
    n_combining <- sum(i_combining)
    bml_new <- vector("list", n_orig - n_combining)
    bml_new[[1]] <-  bml[[1]]
    names(bml_new)[1] <- names(bml)[1]
    i_new <- 1L
    for (i in seq.int(2, n_orig)) {
        if (i_combining[i]) {
            bml_new[[i_new]] <- bm_overlay(bml_new[[i_new]], bml[[i]], ...)
            names(bml_new)[i_new] <- paste(names(bml_new)[i_new], names(bml)[i])
        } else {
            i_new <- i_new + 1L
            bml_new[[i_new]] <- bml[[i]]
            names(bml_new)[i_new] <- names(bml)[i]
        }
    }
    class(bml_new) <- c("bm_list", class(bml_new))
    bml_new
}
