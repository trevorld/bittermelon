is_bm_object <- function(bm_object) {
    is_bm_bitmap(bm_object) || is_bm_list(bm_object)
}

modify_bm_bitmaps <- function(bm_object, fn, ...) {
    stopifnot(is_bm_object(bm_object))
    if (is_bm_list(bm_object)) {
        bm_lapply(bm_object, fn, ...)
    } else {
        fn(bm_object, ...)
    }
}

`%||%` <- function(x, y) if (is.null(x)) y else x # nolint
