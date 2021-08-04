#' max, min, and range for bitmap objects
#'
#' `max()`, `min()`, and `range()` will provide the  maximum and minimum
#' integer values found in the `bm_bitmap()`, `bm_list()`, or `bm_list()` objects.
#' The other four S3 [base::Summary] methods - `all()`, `any()`, `sum`, and `prod` -
#' are only supported for `bm_bitmap()` objects
#' (which are subclasses of integer matrices).
#'
#' @param ... Passed to relevant functions.
#' @param na.rm Passed to `min()` and `max()`.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   min(font)
#'   max(font)
#'   range(font)
#' @return An integer vector.
#' @export
Summary.bm_list <- function(..., na.rm = FALSE) { # nolint
    switch(.Generic,
           "min" = min(sapply(..., function(o) min(o, na.rm = na.rm))),
           "max" = max(sapply(..., function(o) max(o, na.rm = na.rm))),
           "range" = c(min(..., na.rm = na.rm), max(..., na.rm = na.rm)),
           stop(paste0("Summary function '", .Generic, "' not defined for `bm_list()` objects")))
}
