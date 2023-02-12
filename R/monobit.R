#' Read and write bitmap font files using monobit
#'
#' `read_monobit()` reads in bitmap font file as a [bm_font()] object while `write_monobit()`
#' writes a [bm_font()] object as a bitmap font file.
#' It uses the file extension to determine the appropriate bitmap font format to use.
#' `update_monobit()` downloads an updated version of `monobit`.
#'
#' `read_monobit()` and `write_monobit()` require Python v3.6 or greater available on the system.
#' `read_monobit()` and `write_monobit()` uses `monobit`'s `convert.py` script to convert to/from
#' the yaff font format which this package can natively read/write from/to.
#' This package embeds an older, smaller version of `monobit`.
#' Use `update_monobit()` to download a newer, better version of `monobit`
#' (which unfortunately is too large to embed within this package).
#'
#' @param file A character string of a filename.
#' @param font A [bm_font()] object.
#' @param monobit_path Which directory containing `monobit` to use.
#'                     Default will be to look in `file.path(rappdirs::user_config_dir("bittermelon"), "monobit")`,
#'                     `file.path(rappdirs::site_config_dir("bittermelon"), "monobit")`, and
#'                    `system.file("monobit", package = "bittermelon")` (in that order).
#' @param quietly If `TRUE` suppress any standard output/error from `monobit`.
#' @param site If `TRUE` try to install into `rappdirs::site_config_dir("bittermelon")`
#'             instead of `rappdirs::user_config_dir("bittermelon")`.
#'             Note this may require administrator privileges.
#' @examples
#'  \donttest{
#'  if (findpython::can_find_python_cmd(minimum_version = "3.6")) {
#'    try({
#'      font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'      tempfile <- tempfile(fileext = ".hex")
#'      writeLines(readLines(font_file), tempfile)
#'
#'      font <- read_monobit(tempfile)
#'      capital_r <- font[[str2ucp("R")]]
#'      print(capital_r, px = c(".", "#"))
#'
#'      filename <- tempfile(fileext = ".yaff")
#'      write_monobit(font, filename)
#'    })
#'  }
#'  }
#' @seealso [bm_font()] for more information about bitmap font objects.
#'    [read_hex()], [write_hex()], [read_yaff()], [write_yaff()] for pure R bitmap font readers and writers.
#'    For more information about `monobit` see <https://github.com/robhagemans/monobit>.
#' @return `read_monobit()` returns a [bm_font()] object.  `write_monobit()` returns `NULL` invisibly and
#'          as a side effect writes `file`.
#' @rdname monobit
#' @export
read_monobit <- function(file, quietly = FALSE,
                         monobit_path = getOption("bittermelon.monobit_path", NULL)) {
    stopifnot(!missing(file))
    if (quietly)
        stdout <- stderr <- FALSE
    else
        stdout <- stderr <- ""
    python <- findpython::find_python_cmd(minimum_version = "3.6")
    monobit_path <- monobit_path %||% get_monobit_path()
    convert <- file.path(monobit_path, "convert.py")

    tfile <- tempfile(fileext = ".yaff.gz")
    on.exit(unlink(tfile))

    system2(python, c(convert, file, tfile), stdout = stdout, stderr = stderr)
    read_yaff(tfile)
}

get_monobit_path <- function() {
    user_monobit_path <- file.path(rappdirs::user_config_dir("bittermelon"), "monobit")
    if (dir.exists(user_monobit_path))
        return(user_monobit_path)

    site_monobit_path <- file.path(rappdirs::site_config_dir("bittermelon"), "monobit")
    if (dir.exists(site_monobit_path))
            return(site_monobit_path)

    system.file("monobit", package = "bittermelon", mustWork = TRUE)
}

#' @rdname monobit
#' @export
write_monobit <- function(font, file, quietly = FALSE,
                          monobit_path = getOption("bittermelon.monobit_path", NULL)) {
    stopifnot(!missing(font), !missing(file))
    if (quietly)
        stdout <- stderr <- FALSE
    else
        stdout <- stderr <- ""
    python <- findpython::find_python_cmd(minimum_version = "3.6")
    monobit_path <- monobit_path %||% get_monobit_path()
    convert <- file.path(monobit_path, "convert.py")

    tfile <- tempfile(fileext = ".yaff.gz")
    on.exit(unlink(tfile))

    write_yaff(font, gzfile(tfile))
    system2(python, c(convert, tfile, file), stdout = stdout, stderr = stderr)
    invisible(invisible(NULL))
}

#' @rdname monobit
#' @export
update_monobit <- function(site = FALSE) {
    if (!requireNamespace("git2r"))
        stop(paste("Requires suggested package {git2r}.",
                   'Use `install.packages("git2r")` to install it.'))

    if (site)
        monobit_path <- file.path(rappdirs::site_config_dir("bittermelon"), "monobit")
    else
        monobit_path <- file.path(rappdirs::user_config_dir("bittermelon"), "monobit")

    if (dir.exists(monobit_path))
        git2r::pull(monobit_path)
    else
        git2r::clone("https://github.com/robhagemans/monobit.git", monobit_path)
}
