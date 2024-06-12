#' @section Package options:
#'  The following `bittermelon` option may be set globally via [base::options()]:
#'   \describe{
#'     \item{bittermelon.bg}{Set new default `bg` argument value for
#'                           [format.bm_bitmap()], [format.bm_pixmap()], [print.bm_bitmap()],
#'                           and [print.bm_pixmap()].}
#'     \item{bittermelon.col}{Set new default `col` argument value for
#'                           [as.raster.bm_bitmap()] and [plot.bm_bitmap()].}
#'     \item{bittermelon.compress}{Set new default `compress` argument value for
#'                           [format.bm_bitmap()], [format.bm_pixmap()], [print.bm_bitmap()],
#'                           and [print.bm_pixmap()].}
#'     \item{bittermelon.downscale}{Set new default `downscale` argument value for
#'                           [format.bm_bitmap()], [format.bm_pixmap()], [print.bm_bitmap()],
#'                           and [print.bm_pixmap()].}
#'     \item{bittermelon.fg}{Set new default `fg` argument value for
#'                           [format.bm_bitmap()] and [print.bm_bitmap()].}
#'     \item{bittermelon.monobit_path}{Set new default `monobit_path` argument value
#'                           for [read_monobit()] and [write_monobit()].}
#'     \item{bittermelon.px}{Set new default `px` argument value for
#'                           [format.bm_bitmap()] and [print.bm_bitmap()].
#'                           If unset both functions default to [px_auto()].}
#'   }
#'  The following `cli` options may also be of interest:
#'   \describe{
#'     \item{cli.num_colors}{See [cli::num_ansi_colors()].
#'                           If set to `1L` then the `cli` package won't add any ANSI sequences.}
#'     \item{cli.unicode}{Whether UTF-8 character support should be assumed.
#'                        Used by [px_auto()] which is the default of the `bittermelon.px` option.}
#'   }
#' @keywords internal
#' @name bittermelon
"_PACKAGE"
