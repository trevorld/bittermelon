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
#' @param font A [bm_font()] object.
#' @param quietly If `TRUE` suppress any standard output/error from `monobit`.
#' @examples
#'  if (findpython::can_find_python_cmd(minimum_version = "3.6")) {
#'    font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'    tempfile <- tempfile(fileext = ".hex")
#'    writeLines(readLines(font_file), tempfile)
#'
#'    try({ # `monobit` is alpha level software which may not always work
#'      font <- read_monobit(tempfile)
#'      capital_r <- font[[str2ucp("R")]]
#'      print(capital_r, px = c(".", "#"))
#'
#'      filename <- tempfile(fileext = ".yaff")
#'      write_monobit(font, filename)
#'    })
#'  }
#' @seealso [bm_font()] for more information about bitmap font objects.
#'    [read_hex()], [write_hex()], [read_yaff()], [write_yaff()] for pure R bitmap font readers and writers.
#'    For more information about `monobit` see <https://github.com/robhagemans/monobit>.
#' @return `read_monobit()` returns a [bm_font()] object.  `write_monobit()` returns `NULL` invisibly and
#'          as a side effect writes `file`.
#' @rdname monobit
#' @export
read_monobit <- function(file, quietly = FALSE) {
    stopifnot(!missing(file))
    if (quietly)
        stdout <- stderr <- FALSE
    else
        stdout <- stderr <- ""
    python <- findpython::find_python_cmd(minimum_version = "3.6")
    convert <- system.file("monobit/convert.py", package = "bittermelon", mustWork = TRUE)

    tfile <- tempfile(fileext = ".yaff")
    on.exit(unlink(tfile))

    system2(python, c(convert, file, tfile), stdout = stdout, stderr = stderr)
    read_yaff(tfile)
}

#' @rdname monobit
#' @export
write_monobit <- function(font, file, quietly = FALSE) {
    stopifnot(!missing(font), !missing(file))
    if (quietly)
        stdout <- stderr <- FALSE
    else
        stdout <- stderr <- ""
    python <- findpython::find_python_cmd(minimum_version = "3.6")
    convert <- system.file("monobit/convert.py", package = "bittermelon", mustWork = TRUE)

    tfile <- tempfile(fileext = ".yaff")
    on.exit(unlink(tfile))

    write_yaff(font, tfile)
    system2(python, c(convert, tfile, file), stdout = stdout, stderr = stderr)
    invisible(invisible(NULL))
}
