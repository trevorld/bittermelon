#' Read and write bitmap font files using monobit
#'
#' `read_monobit()` reads in bitmap font file as a [bm_font()] object while `write_monobit()`
#' writes a [bm_font()] object as a bitmap font file.
#' It uses the file extension to determine the appropriate bitmap font format to use.
#'
#' * `read_monobit()` and `write_monobit()` require that the `monobit-convert` command is available on the system.
#' * `read_monobit()` and `write_monobit()` uses `monobit-convert` to convert to/from
#'   the yaff font format which this package can natively read/write from/to.
#' * One may install `monobit-convert` using `pip3 install monobit`.
#' * For more information about `monobit` see <https://github.com/robhagemans/monobit>.
#'
#' @param file A character string of a filename.
#' @param font A [bm_font()] object.
#' @param monobit_path Path/name of `monobit-convert` to use.  Passed to [base::Sys.which()].
#' @param quietly If `TRUE` suppress any standard output/error from `monobit-convert`.
#' @examples
#' \donttest{# May take more than 5 seconds on CRAN servers
#' if (Sys.which("monobit-convert") != "") {
#'   try({
#'     font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'     font <- read_monobit(font_file)
#'     capital_r <- font[[str2ucp("R")]]
#'     print(capital_r)
#'
#'     filename <- tempfile(fileext = ".yaff")
#'     write_monobit(font, filename)
#'   })
#' }
#' }
#' @seealso [bm_font()] for more information about bitmap font objects.
#'    [read_hex()], [write_hex()], [read_yaff()], [write_yaff()] for pure R bitmap font readers and writers.
#' @return `read_monobit()` returns a [bm_font()] object.  `write_monobit()` returns `NULL` invisibly and
#'          as a side effect writes `file`.
#' @rdname monobit
#' @export
read_monobit <- function(file, quietly = FALSE,
                         monobit_path = getOption("bittermelon.monobit_path", "monobit-convert")) {
    monobit <- Sys.which(monobit_path)
    stopifnot(!missing(file), monobit != "")
    if (quietly)
        stdout <- stderr <- FALSE
    else
        stdout <- stderr <- ""

    tfile <- tempfile(fileext = ".yaff.gz")
    on.exit(unlink(tfile))

    system2(monobit, c(file, tfile), stdout = stdout, stderr = stderr)
    read_yaff(tfile)
}

#' @rdname monobit
#' @export
write_monobit <- function(font, file, quietly = FALSE,
                          monobit_path = getOption("bittermelon.monobit_path", "monobit-convert")) {
    monobit <- Sys.which(monobit_path)
    stopifnot(!missing(font), !missing(file), monobit != "")
    if (quietly)
        stdout <- stderr <- FALSE
    else
        stdout <- stderr <- ""

    tfile <- tempfile(fileext = ".yaff.gz")
    on.exit(unlink(tfile))

    write_yaff(font, gzfile(tfile))
    system2(monobit, c(tfile, file), stdout = stdout, stderr = stderr)
    invisible(NULL)
}
