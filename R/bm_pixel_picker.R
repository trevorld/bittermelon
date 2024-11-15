#' Bitmap pixel picker
#'
#' `bm_pixel_picker()` lets you use an interactive graphics device
#' to click on a bitmap's pixels and learn the
#' column/row coordinates for the clicked pixel and its integer/color value.
#' To end the program click a non-left mouse button within the graphics device.
#' @param x Either a [bm_bitmap()], [bm_pixmap()],
#'          or [raster][grDevices::as.raster()]  object
#' @param ... Currently ignored.
#' @inheritParams as.raster.bm_bitmap
#' @param silent Don't generate messages about clicked pixels.
#' @return A list with named components "row", "col", and "value" 
#'         for the last clicked pixel returned invisibly.
#' @examples
#' if (interactive() && dev.interactive(orNone = TRUE)) {
#'   corn <- farming_crops_16x16()$corn$portrait
#'   bm_pixel_picker(corn)
#' }
#' @seealso This function wraps [grid::grid.locator()].
#' @export
bm_pixel_picker <- function(x, ...) {
    UseMethod("bm_pixel_picker")
}

#' @rdname bm_pixel_picker 
#' @export
bm_pixel_picker.bm_bitmap <- function(x, ..., 
                                      col = getOption("bittermelon.col", col_bitmap),
                                      silent = FALSE) {
    stopifnot(grDevices::dev.interactive(orNone = TRUE))
    nc <- bm_widths(x)
    nr <- bm_heights(x)
    env <- list2env(list(row = NA_integer_, col = NA_integer_, value = NA_character_))
    bm_pixel_picker_plot(x, nc, nr, col = col)
    tryCatch(get_picks(x, env, nc, nr, silent), 
        error = function(e) invisible(NULL))
    invisible(as.list(env))
}

#' @rdname bm_pixel_picker 
#' @export
bm_pixel_picker.bm_pixmap <- function(x, ..., silent = FALSE) {
    stopifnot(grDevices::dev.interactive(orNone = TRUE))
    nc <- bm_widths(x)
    nr <- bm_heights(x)
    env <- list2env(list(row = NA_integer_, col = NA_integer_, value = NA_character_))
    bm_pixel_picker_plot(x, nc, nr)
    tryCatch(get_picks(x, env, nc, nr, silent), 
        error = function(e) invisible(NULL))
    invisible(as.list(env))
}

#' @rdname bm_pixel_picker 
#' @export
bm_pixel_picker.raster <- function(x, ..., silent = FALSE) {
    stopifnot(grDevices::dev.interactive(orNone = TRUE))
    nc <- bm_widths(x)
    nr <- bm_heights(x)
    env <- list2env(list(row = NA_integer_, col = NA_integer_, value = NA_character_))
    bm_pixel_picker_plot(x, nc, nr, first_row_is_top = TRUE)
    tryCatch(get_picks(x, env, nc, nr, silent), 
        error = function(e) invisible(NULL))
    invisible(as.list(env))
}

bm_pixel_picker_plot <- function(x, nc = bm_widths(x), nr = bm_heights(x),
                         ..., first_row_is_top = FALSE) {
    vp <- bm_viewport(x, nc, nr, first_row_is_top)
    grid::grid.newpage()
    grid::pushViewport(vp)
    grid::grid.raster(as.raster(x, ...), interpolate = FALSE)
    invisible(NULL)
}

get_picks <- function(x, env, nc = bm_widths(x), nr = bm_heights(x), silent = FALSE) {
    add_color <- cli::is_utf8_output() && cli::num_ansi_colors() >= 256L
    xy_native <- grid::grid.locator("native")
    while (!is.null(xy_native)) {
        col <- as.integer(ceiling(as.numeric(xy_native$x)))
        row <- as.integer(ceiling(as.numeric(xy_native$y)))
        if (1 <= col && col <= nc && 1 <= row && row <= nr) {
            env$col <- col
            env$row <- row
            env$value <- x[row, col]
            msg <- paste("value:", env$value, "row:", env$row, "col:", env$col)
            if (add_color && is.character(env$value))
                msg <- paste(cli::make_ansi_style(env$value)("\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588"), msg)

            if (isFALSE(silent))
                message(msg)
        }
        xy_native <- grid::grid.locator("native")
    }
    invisible(NULL)
}

bm_viewport <- function(x, 
                        pixel_width = bm_widths(x),
                        pixel_height = bm_heights(x),
                        first_row_is_top = FALSE) {
    pixel_length <- max(pixel_width, pixel_height)
    snpc_width <- 1.0 * pixel_width / pixel_length
    snpc_height <- 1.0 * pixel_height / pixel_length
    if (first_row_is_top)
        yscale <- c(pixel_height, 0)
    else
        yscale <- c(0, pixel_height)
    grid::viewport(x = grid::unit(0.5, "npc"),
        y = grid::unit(0.5, "npc"),
        width = grid::unit(snpc_width, "snpc"),
        height = grid::unit(snpc_height, "snpc"),
        xscale = c(0, pixel_width),
        yscale = yscale)
}
