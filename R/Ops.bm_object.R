#' S3 Ops group generic methods for bitmap objects
#'
#' The S3 Ops group generic methods for `bm_bitmap()` objects
#' are simply the result of the generic integer matrix method
#' cast back to a `bm_bitmap()` object (which is an integer matrix).
#' The S3 Ops group generic methods for `bm_list()`
#' and `bm_font()` objects simply returns another object
#' with that operator applied to every bitmap in the original object.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#'
#' # Examples applied to individual bitmaps
#' capital_r <- font[[str2ucp("R")]]
#' print(!capital_r)
#' capital_b <- font[[str2ucp("B")]]
#' print(capital_r & capital_b)
#' print(capital_r | capital_b)
#' print(capital_r + 1L)
#' print(capital_r + 1L > 1L)
#'
#' # Examples applied to `bm_list()` objects
#' bml <- font[c("U+0023", "U+0052", "U+0053", "U+0054", "U+0041", "U+0054", "U+0053")] # #RSTATS
#' bml <- as_bm_list(bml)
#' bm <- do.call(cbind, bml)
#' print(bm)
#'
#' bml <- !bml
#' bm <- do.call(cbind, bml)
#' print(bm)
#'
#' bml <- 2 * (bml + 1L)
#' bm <- do.call(cbind, bml)
#' print(bm)
#'
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' print(corn == col2hex("transparent"))
#' @rdname Ops.bm_object
#' @param e1,e2 objects.
#' @return The various `Ops.bm_bitmap` and `Ops.bm_pixmap` methods return a [bm_bitmap()] object.
#'         The various `Ops.bm_list` methods return a [bm_list()] object.
#' @seealso [base::Ops]
#' @export
Ops.bm_bitmap <- function(e1, e2) {
   as_bm_bitmap.matrix(NextMethod())
}


#' @rdname Ops.bm_object
#' @export
Ops.bm_pixmap <- function(e1, e2) {
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
