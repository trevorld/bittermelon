#' @section Package options:
#'  The following `bittermelon` option may be set globally via [base::options()]:
#'   \describe{
#'     \item{bittermelon.px}{Set new default `px` argument value for
#'                           [format.bm_bitmap()] and [print.bm_bitmap()].
#'                           If unset both functions default to `px_unicode` but
#'                           some may prefer `px_ascii` which is an ASCII alternative.}
#'     \item{bittermelon.fg}{Set new default `fg` argument value for
#'                           [format.bm_bitmap()] and [print.bm_bitmap()].}
#'     \item{bittermelon.bg}{Set new default `bg` argument value for
#'                           [format.bm_bitmap()] and [print.bm_bitmap()].}
#'     \item{bittermelon.compress}{Set new default `compress` argument value for
#'                           [format.bm_bitmap()] and [print.bm_bitmap()].}
#'     \item{bittermelon.monobit_path}{Set new default `monobit_path` argument value
#'                           for [read_monobit()] and [write_monobit()].}
#'   }
#'  The following `findpython` option may also be of interest:
#'   \describe{
#'     \item{python_cmd}{Explicitly set path to python binary to use with [read_monobit()]
#'                       or [write_monobit()].  This is actually the appropriate global option
#'                       for [findpython::find_python_cmd()].}
#'   }
#' @keywords internal
#' @name bittermelon
"_PACKAGE"
