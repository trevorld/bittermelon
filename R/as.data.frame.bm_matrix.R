#' Convert to data frame with pixel (x,y) coordinates
#'
#' `as.matrix.bm_matrix()` casts [bm_bitmap()] objects to a (normal) integer matrix
#' and [bm_pixmap()] objects to a (normal) character matrix (of color strings).
#' @param x Either a [bm_bitmap()] or [bm_pixmap()] object.
#' @param ... Currently ignored.
#' @param filtrate If `FALSE` (default) get coordinates for all values.
#'   If a single value only return the coordinates for pixels that equal that value.
#' @examples
#' font_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
#' font <- read_yaff(font_file)
#' bm <- as_bm_bitmap("RSTATS", font = font)
#' df <- as.data.frame(bm, filtrate = 1L)
#' if (require("grid")) {
#'   grid.newpage()
#'   grid.rect(df$x * 0.6, df$y * 0.6, width = 0.5, height = 0.5,
#'             gp = gpar(fill = 'black'), default.units = 'cm')
#' }
#'
#' corn <- farming_crops_16x16()$corn$portrait
#' df <- as.data.frame(corn)
#' if (require("grid")) {
#'   grid.newpage()
#'   grid.circle(df$x * 0.6, df$y * 0.6, r = 0.25,
#'               gp = gpar(fill = df$value), default.units = 'cm')
#' }
#' @return A data frame with "x", "y", and "value" columns.
#' @rdname as.data.frame.bm_matrix
#' @export
as.data.frame.bm_bitmap <- function(x, ..., filtrate = FALSE) {
    if (isFALSE(filtrate)) {
        df <- data.frame(
          x = rep(seq_len(ncol(x)), each = nrow(x)),
          y = rep.int(seq_len(nrow(x)), times = ncol(x)),
          value = as.integer(x)
        )
    } else {
        filtrate <- as.integer(filtrate)
        mat <- arrayInd(which(as.logical(x == filtrate)), dim(x),
                        .dimnames = list(y=NULL, x=NULL), useNames = TRUE)
        df <- as.data.frame(mat)[, c("x", "y")]
        df$value <- rep_len(filtrate, nrow(mat))
    }
    df
}

#' @rdname as.data.frame.bm_matrix
#' @export
as.data.frame.bm_pixmap <- function(x, ..., filtrate = FALSE) {
    if (isFALSE(filtrate)) {
        df <- data.frame(
          x = rep(seq_len(ncol(x)), each = nrow(x)),
          y = rep.int(seq_len(nrow(x)), times = ncol(x)),
          value = as.character(x)
        )
    } else {
        filtrate <- col2hex(filtrate)
        mat <- arrayInd(which(as.logical(x == filtrate)), dim(x),
                        .dimnames = list(y=NULL, x=NULL), useNames = TRUE)
        df <- as.data.frame(mat)[, c("x", "y")]
        df$value <- rep_len(filtrate, nrow(mat))
    }
    df
}
