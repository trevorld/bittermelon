#' Combine bitmap objects
#'
#' `c()` combines bitmap objects into [bm_list()] or [bm_font()] objects.
#'  In particular when using it to combine fonts the later fonts
#'  "update" the glyphs in the earlier fonts.
#'
#' The various bitmap objects are "reduced" in the following ways:
#' \tabular{lll}{
#' First \tab Second \tab Result\cr
#' [bm_bitmap()] \tab [bm_bitmap()] \tab [bm_list()]\cr
#' [bm_bitmap()] \tab [bm_font()] \tab [bm_font()]\cr
#' [bm_bitmap()] \tab [bm_list()] \tab [bm_list()]\cr
#' [bm_bitmap()] \tab [bm_pixmap()] \tab [bm_list()]\cr
#' [bm_pixmap()] \tab [bm_bitmap()] \tab [bm_list()]\cr
#' [bm_pixmap()] \tab [bm_font()] \tab `ERROR`\cr
#' [bm_pixmap()] \tab [bm_list()] \tab [bm_list()]\cr
#' [bm_pixmap()] \tab [bm_pixmap()] \tab [bm_list()]\cr
#' [bm_font()] \tab [bm_bitmap()] \tab [bm_font()]\cr
#' [bm_font()] \tab [bm_font()] \tab [bm_font()]\cr
#' [bm_font()] \tab [bm_list()] \tab [bm_font()]\cr
#' [bm_font()] \tab [bm_pixmap()] \tab `ERROR`\cr
#' [bm_list()] \tab [bm_bitmap()] \tab [bm_list()]\cr
#' [bm_list()] \tab [bm_font()] \tab [bm_font()]\cr
#' [bm_list()] \tab [bm_list()] \tab [bm_list()]\cr
#' [bm_list()] \tab [bm_pixmap()] \tab [bm_list()]\cr
#' }
#' When combining with a `bm_font()` object if any `bm_bitmap()` objects
#' share the same name we only keep the last one.
#' Although names are preserved other attributes such as font
#' comments and properties are not guaranteed to be preserved.
#' @return Either a [bm_list()] or [bm_font()] object.
#'         See Details for more info.
#' @param ... [bm_bitmap()], [bm_list()], and/or [bm_font()] objects to combine.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' stats <- as_bm_list("STATS", font = font)
#' is_bm_list(c(capital_r, capital_r))
#' rstats <- c(capital_r, stats)
#' print(bm_call(rstats, cbind))
#' @rdname combine
#' @export
c.bm_bitmap <- function(...) {
    l <- list(...)
    stopifnot(length(l) > 0L)
    if (length(l) == 1L) {
        r <- bm_list(...)
    } else {
        second <- l[[2L]]
        stopifnot(is_bm_matrix(second) || is_bm_list(second))
        if (is_bm_matrix(second)) {
            r <- as_bm_list.list(l[1:2])
        } else if (is_bm_font(second)) {
            r <- c.bm_font(as_bm_font(as_bm_list.list(l[1L])), second)
        } else {
            r <- c.bm_list(as_bm_list.list(l[1L]), second)
        }
    }
    if (length(l) > 2L) {
        do.call(c, c(list(r), l[c(-1L, -2L)]))
    } else {
        r
    }
}

#' @rdname combine
#' @export
c.bm_font <- function(...) {
    l <- list(...)
    stopifnot(length(l) > 0L)
    if (length(l) == 1L) {
        r <- l[[1]]
    } else {
        r <- l[[1]]
        second <- l[[2L]]
        stopifnot(is_bm_bitmap(second) || is_bm_list(second))
        if (is_bm_bitmap(second)) {
            ucp <- names(l)[2L]
            stopifnot(!is.null(ucp), is_ucp(ucp))
            r[[ucp]] <- l[[2]]
        } else {
            ucps <- unique(names(l[[2]]))
            stopifnot(!is.null(ucps), all(is_ucp(ucps)))
            for (ucp in ucps)
                r[[ucp]] <- l[[2]][[ucp]]
        }
    }
    if (length(l) > 2L) {
        do.call(c, c(list(r), l[c(-1L, -2L)]))
    } else {
        r
    }
}

#' @rdname combine
#' @export
c.bm_list <- function(...) {
    l <- list(...)
    stopifnot(length(l) > 0L)
    if (length(l) == 1L) {
        r <- l[[1]]
    } else {
        second <- l[[2L]]
        stopifnot(is_bm_matrix(second) || is_bm_list(second))
        if (is_bm_matrix(second)) {
            r <- c.bm_list(l[[1]], as_bm_list.list(l[2L]))
        } else if (is_bm_font(second)) {
            r <- c(as.list(l[[1]]), as.list(l[[2]]))
            r <- r[unique(names(r))]
            r <- as_bm_font.list(r)
        } else {
            r <- as_bm_list.list(c(as.list(l[[1]]), as.list(l[[2]])))
        }
    }
    if (length(l) > 2L) {
        do.call(c, c(list(r), l[c(-1L, -2L)]))
    } else {
        r
    }
}

#' @rdname combine
#' @export
c.bm_pixmap <- function(...) {
    l <- list(...)
    stopifnot(length(l) > 0L)
    if (length(l) == 1L) {
        r <- bm_list(...)
    } else {
        second <- l[[2L]]
        stopifnot(is_bm_matrix(second) || is_bm_list(second),
                  !is_bm_font(second))
        if (is_bm_matrix(second)) {
            r <- as_bm_list.list(l[1:2])
        } else {
            r <- c.bm_list(as_bm_list.list(l[1L]), second)
        }
    }
    if (length(l) > 2L) {
        do.call(c, c(list(r), l[c(-1L, -2L)]))
    } else {
        r
    }
}
