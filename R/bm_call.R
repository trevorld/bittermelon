#' Execute a function call on bitmap objects
#'
#' `bm_call()` excutes a function call on bitmap objects.
#' Since its first argument is the bitmap object it is more
#' convenient to use with pipes then directly using [base::do.call()]
#' plus it is easier to specify additional arguments.
#' @inheritParams bm_clamp
#' @param .f A function to execute.
#' @param ... Additional arguments to `.f`.
#' @return The return value of `.f`.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   bml <- as_bm_list("RSTATS", font = font)
#'   bml <- bm_flip(bml, "both")
#'   bm <- bm_call(bml, cbind, direction = "RTL")
#'   print(bm, px = px_ascii)
#' @export
bm_call <- function(bm_object, .f, ...) {
    if (!is.list(bm_object))
        bm_object <- list(bm_object)
    l <- c(as.list(bm_object), list(...))
    do.call(.f, l)
}
