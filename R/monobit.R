#' Read and write bitmap font files using monobit
#'
#' `read_monobit()` reads in bitmap font file as a [bm_font()] object while `write_monobit()`
#' writes a [bm_font()] object as a bitmap font file.
#' It uses the file extension to determine the appropriate bitmap font format to use.
#'
#' `read_monobit()` and `write_monobit()` require Python v3.6 or greater available on the system.
#' `read_monobit()` and `write_monobit()` uses `monobit`'s `convert.py` script to convert to/from
#' the yaff font format which this package can natively read/write from/to.
#' Note `monobit` is alpha level software which may not always work
#' (in particular font export may be buggy).
#'
#' @param file A character string of a filename.
#'
#' @param font A [bm_font()] object.
#' @examples
#'  if (findpython::can_find_python_cmd(minimum_version = "3.6")) {
#'    font_file <- system.file("fonts/spleen/spleen-8x16.yaff.gz", package = "bittermelon")
#'    tempfile <- tempfile(fileext = ".yaff")
#'    writeLines(readLines(font_file), tempfile)
#'
#'    font <- read_monobit(tempfile)
#'    capital_r <- font[[str2ucp("R")]]
#'    print(capital_r, labels = c(".", "#"))
#'
#'    filename <- tempfile(fileext = ".yaff")
#'    write_monobit(font, filename)
#'  }
#' @seealso [bm_font()] for more information about bitmap font objects.
#'    [read_hex()], [write_hex()], [read_yaff()], [write_yaff()] for pure R bitmap font readers and writers.
#'    For more information about `monobit` see <https://github.com/robhagemans/monobit>.
#' @rdname monobit
#' @export
read_monobit <- function(file) {
    stopifnot(!missing(file))
    python <- findpython::find_python_cmd(minimum_version = "3.6")
    convert <- system.file("monobit/convert.py", package = "bittermelon", mustWork = TRUE)

    tfile <- tempfile(fileext = ".yaff")
    on.exit(unlink(tfile))

    system2(python, c(convert, file, tfile))
    read_yaff(tfile)
}

#' @rdname monobit
#' @export
write_monobit <- function(font, file) {
    stopifnot(!missing(font), !missing(file))
    python <- findpython::find_python_cmd(minimum_version = "3.6")
    convert <- system.file("monobit/convert.py", package = "bittermelon", mustWork = TRUE)

    tfile <- tempfile(fileext = ".yaff")
    on.exit(unlink(tfile))

    write_yaff(font, tfile)
    system2(python, c(convert, tfile, file))
    invisible(invisible(NULL))
}