#' S3 Ops group generic methods for bitmap objects
#'
#' The S3 Ops group generic methods for `bm_bitmap()` objects
#' are simply the result of the generic integer matrix method
#' cast back to a binary `bm_bitmap()` object (which
#' is an integer matrix of ones and zeros).
#' The S3 Ops group generic methods for `bm_list()`
#' and `bm_font()` objects simply returns another object
#' with that operator applied to every bitmap in the original object.
#' Since [base::which()] does not automatically cast
#' its argument to a logical value we also redefine it as a generic
#' and besides a default method which simply calls `base:which()` we
#' offer a `which.bm_bitmap()` method that first
#' casts the bitmap to logical before calling `base::which()`.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'
#'   # Examples applied to individual bitmaps
#'   capital_r <- font[[str2ucp("R")]]
#'   print(!capital_r, px = px_ascii)
#'   capital_b <- font[[str2ucp("B")]]
#'   print(capital_r & capital_b, px = px_ascii)
#'   print(capital_r | capital_b, px = px_ascii)
#'   print(capital_r + 1L, px = px_ascii)
#'   print(capital_r + 1L > 1L, px = px_ascii)
#'   which(capital_r > 0L)
#'
#'   # Examples applied to `bm_list()` objects
#'   bml <- font[c("U+0023", "U+0052", "U+0053", "U+0054", "U+0041", "U+0054", "U+0053")] # #RSTATS
#'   bml <- as_bm_list(bml)
#'   bm <- do.call(cbind, bml)
#'   print(bm, px = px_ascii)
#'
#'   bml <- !bml
#'   bm <- do.call(cbind, bml)
#'   print(bm, px = px_ascii)
#'
#'   bml <- 2 * (bml + 1L)
#'   bm <- do.call(cbind, bml)
#'   print(bm, px = px_ascii)
#'
#' @inheritParams base::Ops
#' @rdname Ops.bm_object
#' @return `which.bm_bitmap()` returns a logical vector.
#'         The various `Ops.bm_bitmap` methods return a [bm_bitmap()] object.
#'         The various `Ops.bm_list` methods return a [bm_list()] object.
#' @seealso [base::Ops]
#' @export
Ops.bm_bitmap <- function(e1, e2) {
   as_bm_bitmap.matrix(NextMethod())
}

#' @rdname Ops.bm_object
#' @export
Ops.bm_list <- function(e1, e2) {
    if (missing(e2)) {
        switch(.Generic,
               "!" = bm_lapply(e1, function(o) !o),
               "+" = e1,
               "-" = bm_lapply(e1, function(o) -o),
               stop(paste0("unary operation '", .Generic, "' not defined for `bm_list()` objects")))
    } else {
        if (is_bm_list(e1)) {
            bml <- e1
            n <- e2
            switch(.Generic,
                   "+" = bm_lapply(bml, function(o) o + n),
                   "-" = bm_lapply(bml, function(o) o - n),
                   "*" = bm_lapply(bml, function(o) o * n),
                   "/" = bm_lapply(bml, function(o) o / n),
                   "^" = bm_lapply(bml, function(o) o ^ n),
                   "%%" = bm_lapply(bml, function(o) o %% n),
                   "%/%" = bm_lapply(bml, function(o) o %/% n),
                   "&" = bm_lapply(bml, function(o) o & n),
                   "|" = bm_lapply(bml, function(o) o | n),
                   "==" = bm_lapply(bml, function(o) o == n),
                   "!=" = bm_lapply(bml, function(o) o != n),
                   "<" = bm_lapply(bml, function(o) o < n),
                   "<=" = bm_lapply(bml, function(o) o <= n),
                   ">=" = bm_lapply(bml, function(o) o >= n),
                   ">" = bm_lapply(bml, function(o) o > n),
                   stop(paste0("binary operation '", .Generic, "' not defined for `bm_list()` objects")))
        } else {
            n <- e1
            bml <- e2
            switch(.Generic,
                   "+" = bm_lapply(bml, function(o) n + o),
                   "-" = bm_lapply(bml, function(o) n - o),
                   "*" = bm_lapply(bml, function(o) n * o),
                   "/" = bm_lapply(bml, function(o) n / o),
                   "^" = bm_lapply(bml, function(o) n ^ o),
                   "%%" = bm_lapply(bml, function(o) n %% o),
                   "%/%" = bm_lapply(bml, function(o) n %/% o),
                   "&" = bm_lapply(bml, function(o) n & o),
                   "|" = bm_lapply(bml, function(o) n | o),
                   "==" = bm_lapply(bml, function(o) n == o),
                   "!=" = bm_lapply(bml, function(o) n != o),
                   "<" = bm_lapply(bml, function(o) n < o),
                   "<=" = bm_lapply(bml, function(o) n <= o),
                   ">=" = bm_lapply(bml, function(o) n >= o),
                   ">" = bm_lapply(bml, function(o) n > o),
                   stop(paste0("binary operation '", .Generic, "' not defined for `bm_list()` objects")))
        }
    }
}

#' @inheritParams base::which
#' @rdname Ops.bm_object
#' @export
which <- function(x, arr.ind = FALSE, useNames = TRUE) { # nolint
    UseMethod("which")
}

#' @rdname Ops.bm_object
#' @export
which.default <- function(x, arr.ind = FALSE, useNames = TRUE) { # nolint
    base::which(x, arr.ind, useNames)
}

#' @rdname Ops.bm_object
#' @export
which.bm_bitmap <- function(x, arr.ind = FALSE, useNames = TRUE) { # nolint
    base::which(as.logical(x))
}
