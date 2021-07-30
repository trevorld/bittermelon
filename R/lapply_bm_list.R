#' Apply a function over a bitmap glyph list
#'
#' `lapply_bm_list()` applies a function over a bitmap glyph list
#' and returns a modified bitmap glyph list.
#'
#' `lapply_bm_list()` is a wrapper around `base::lapply()` that
#' preserves the classes and metadata of the original bitmap glyph list.
#' @param X A bitmap glyph list object such as [bm_list()] or [bm_font()].
#' @param FUN A function that takes a [bm_glyph()] object as its first argument
#'            and returns a [bm_glyph()] object.
#' @param ... Additional arguments to pass to `FUN`.
#' @return A modified bitmap glyph list.
#' @seealso [base::lapply()], [bm_list()], [bm_font()], [bm_glyph()]
#' @export
lapply_bm_list <- function(X, FUN, ...) { # nolint
    l2 <- lapply(X, FUN, ...)
    class(l2) <- class(X)
    attr(l2, "metadata") <- attr(X, "metadata")
    l2
}
