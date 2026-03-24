#' Pad bitmap widths to the nearest byte
#'
#' `bm_bytepad()` pads the width of bitmaps to the nearest multiple of 8
#' by padding with pixels on the right.
#'
#' This is required by the BDF font format, which specifies that each row of
#' bitmap data must be padded with zero bits on the right to the nearest byte.
#' @inheritParams bm_extend
#' @param ... Passed to [bm_extend()] along with `hjust = "left"` and `width_multiples_of = 8L`.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' ncol(capital_r) # already 8 pixels wide
#' capital_r_7 <- bm_trim(capital_r, right = 1L)
#' ncol(capital_r_7) # trimmed to 7 pixels wide
#' capital_r_8 <- bm_bytepad(capital_r_7)
#' ncol(capital_r_8) # padded back to 8 pixels wide
#' all.equal(capital_r, capital_r_8)
#' @seealso [bm_extend()]
#' @inherit bm_clamp return
#' @export
bm_bytepad <- function(x, ...) {
	bm_extend(x, ..., width_multiples_of = 8L, hjust = "left")
}
