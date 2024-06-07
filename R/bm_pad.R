#'  Adjust bitmap padding lengths
#'
#' `bm_pad()` adjusts bitmap padding lengths.
#'
#' @inheritParams bm_extend
#' @param type Either "exact", `"extend", or "trim".
#'             "exact" makes sure the padding is exactly the indicated amount,
#'             "extend" does not trim any padding if existing padding is more than the indicated amount,
#'             and "trim" does not extend any padding if existing padding is less than the indicated amount.
#' @param sides If not `NULL` then an integer vector indicating the desired
#'              number of pixels of padding on all four sides.
#'              If the integer vector is of length one it indicates the number of pixels for all four sides.
#'              If of length two gives first the number for the vertical sides and then the horizontal sides.
#'              If of length three gives the number of pixels for top, the horizontal sides, and then bottom sides.
#'              If of length four gives the number of pixels for top, right, bottom, and then left sides.
#'              This is the same scheme as used by the CSS padding and margin properties.
#' @param top Desired number of pixels of padding on the top.
#' @param right Desired number of pixels of padding on the right.
#' @param bottom Desired number of pixels of padding on the bottom.
#' @param left Desired number of pixels of padding on the left.
#' @examples
#' font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#' font <- read_hex(font_file)
#' capital_r <- font[[str2ucp("R")]]
#' print(capital_r)
#' capital_r_padded <- bm_pad(capital_r, sides = 2L)
#' print(capital_r_padded)
#' crops <- farming_crops_16x16()
#' corn <- crops$corn$portrait
#' corn_pad4 <- bm_pad(corn, sides = 4L)
#' if (cli::is_utf8_output() && cli::num_ansi_colors() >= 256L) {
#'   print(corn_pad4, compress = "v", bg = "cyan")
#' }
#' @seealso [bm_extend()], [bm_resize()], and [bm_trim()]
#' @inherit bm_clamp return
#' @export
bm_pad <- function(x, value,
                   type = c("exact", "extend", "trim"),
                   sides = NULL,
                   top = NULL, right = NULL, bottom = NULL, left = NULL) {
    stopifnot(missing(sides) || missing(top))
    stopifnot(missing(sides) || missing(right))
    stopifnot(missing(sides) || missing(bottom))
    stopifnot(missing(sides) || missing(left))
    UseMethod("bm_pad")
}

#' @rdname bm_pad
#' @export
bm_pad.bm_bitmap <- function(x, value = 0L,
                             type = c("exact", "extend", "trim"),
                             sides = NULL,
                             top = NULL, right = NULL, bottom = NULL, left = NULL) {
    bm_pad_bitmap(x, value = value, type = type, sides = sides,
                  top = top, right = right, bottom = bottom, left = left)
}

#' @rdname bm_pad
#' @export
bm_pad.bm_list <- function(x, ...) {
    bm_lapply(x, bm_pad, ...)
}

#' @rdname bm_pad
#' @export
bm_pad.bm_pixmap <- function(x, value = col2hex("transparent"),
                             type = c("exact", "extend", "trim"),
                             sides = NULL,
                             top = NULL, right = NULL, bottom = NULL, left = NULL) {
    value <- col2hex(value)
    bm_pad_bitmap(x, value = value, type = type, sides = sides,
                  top = top, right = right, bottom = bottom, left = left)
}

#' @rdname bm_pad
#' @export
`bm_pad.magick-image` <- function(x, value = "transparent",
                                  type = c("exact", "extend", "trim"),
                                  sides = NULL,
                                  top = NULL, right = NULL, bottom = NULL, left = NULL) {
    stopifnot(requireNamespace("magick", quietly = TRUE))
    bm_pad_bitmap(x, value = value, type = type, sides = sides,
                  top = top, right = right, bottom = bottom, left = left)
}

#' @rdname bm_pad
#' @export
bm_pad.nativeRaster <- function(x, value = col2int("transparent"),
                                type = c("exact", "extend", "trim"),
                                sides = NULL,
                                top = NULL, right = NULL, bottom = NULL, left = NULL) {
    bm_pad_bitmap(x, value = value, type = type, sides = sides,
                  top = top, right = right, bottom = bottom, left = left)
}

#' @rdname bm_pad
#' @export
bm_pad.raster <- function(x, value = "transparent",
                             type = c("exact", "extend", "trim"),
                             sides = NULL,
                             top = NULL, right = NULL, bottom = NULL, left = NULL) {
    bm_pad_bitmap(x, value = value, type = type, sides = sides,
                  top = top, right = right, bottom = bottom, left = left)
}

bm_pad_bitmap <- function(x, value,
                          type = type, sides = NULL,
                          top = NULL, right = NULL,
                          bottom = NULL, left = NULL) {
    type <- match.arg(type, c("exact", "extend", "trim"))
    if (type %in% c("exact", "extend"))
        x <- bm_pad_extend(x, value = value, sides = sides,
                           top = top, right = right, bottom = bottom, left = left)
    if (type %in% c("exact", "trim"))
        x <- bm_pad_trim(x, value = value, sides = sides,
                         top = top, right = right, bottom = bottom, left = left)
    x
}

bm_pad_extend <- function(x, value, sides = NULL,
                   top = NULL, right = NULL, bottom = NULL, left = NULL) {

    pl <- bm_padding_lengths(x, value = value)
    d <- list(top = top %||% pl[1L], right = right %||% pl[2L],
              bottom = bottom %||% pl[3L], left = left %||% pl[4L])

    if (!is.null(sides))
        d <- adjust_d_sides(sides, d)

    d$top <- ifelse(d$top > pl[1], d$top - pl[1], 0L)
    d$right <- ifelse(d$right > pl[2], d$right - pl[2], 0L)
    d$bottom <- ifelse(d$bottom > pl[3], d$bottom - pl[3], 0L)
    d$left <- ifelse(d$left > pl[4], d$left - pl[4], 0L)

    bm_extend(x, value = value,
              top = d$top, right = d$right, bottom = d$bottom, left = d$left)
}

bm_pad_trim <- function(x, value, sides = NULL,
                        top = NULL, right = NULL, bottom = NULL, left = NULL) {

    pl <- bm_padding_lengths(x, value = value)
    d <- list(top = top %||% pl[1L], right = right %||% pl[2L],
              bottom = bottom %||% pl[3L], left = left %||% pl[4L])

    if (!is.null(sides))
        d <- adjust_d_sides(sides, d)

    d$top <- ifelse(d$top < pl[1], pl[1] - d$top, 0L)
    d$right <- ifelse(d$right < pl[2], pl[2] - d$right, 0L)
    d$bottom <- ifelse(d$bottom < pl[3], pl[3] - d$bottom, 0L)
    d$left <- ifelse(d$left < pl[4], pl[4] - d$left, 0L)

    bm_trim(x, top = d$top, right = d$right, bottom = d$bottom, left = d$left)
}
