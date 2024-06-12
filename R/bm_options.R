#' Get bittermelon options
#'
#' `bm_options()` returns the `bittermelon` packages global options.
#'
#' @param ... `bittermelon` package options using `name = value`.  
#'            The return list will use any of these instead of the current/default values.
#' @param default If `TRUE` return the default values instead of current values.
#' @return A list of option values.
#'         Note this function **does not** set option values itself but
#'         this list can be passed to [options()], [withr::local_options()], or [withr::with_options()].
#' @examples
#'   bm_options()
#'
#'   bm_options(default = TRUE)
#'
#'   bm_options(bittermelon.compress = "vertical")
#' @seealso [bittermelon] for a high-level description of relevant global options.
#' @export
bm_options <- function(..., default = FALSE) {
    bmo <- list(bittermelon.bg = FALSE,
                bittermelon.col = col_bitmap,
                bittermelon.compress = "none",
                bittermelon.downscale = FALSE,
                bittermelon.fg = FALSE,
                bittermelon.monobit_path = "monobit-convert",
                bittermelon.px = px_auto())
    l <- list(...)
    stopifnot(all(names(l) %in% names(bmo)))
    if (isFALSE(default)) {
       for (n in names(bmo)) {
           bmo[n] <- list(getOption(n, bmo[[n]]))
       }
    }
    if (length(names(l))) {
        for (n in names(l)) {
            bmo[n] <- l[n]
        }
    }
    bmo
}
