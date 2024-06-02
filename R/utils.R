modify_bm_bitmaps <- function(x, fn, ...) {
    stopifnot(is_bm_list(x) || is_bm_matrix(x))
    if (is_bm_list(x)) {
        bm_lapply(x, fn, ...)
    } else {
        fn(x, ...)
    }
}

`%||%` <- function(x, y) if (is.null(x)) y else x # nolint
