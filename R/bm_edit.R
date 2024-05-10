#' Edit a bitmap via text editor
#'
#' Edit a binary bitmap in a text editor.
#'
#' Represent zeroes with a `.` and ones with a `@`
#' (as in the `yaff` font format).  You may
#' also add/delete rows/columns but the bitmap must
#' be rectangular.
#'
#' @param bitmap [bm_bitmap()] object.
#'               It will be coerced into a binary bitmap via [bm_clamp()].
#' @param editor Text editor.  See [utils::file.edit()] for more information.
#' @examples
#'   font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'   font <- read_hex(font_file)
#'   r <- font[[str2ucp("R")]]
#'
#'   # requires users to manually close file in text editor
#'   \dontrun{
#'     edited_r <- bm_edit(r)
#'     print(edited_r)
#'   }
#' @return A [bm_bitmap()] object.
#' @export
bm_edit <- function(bitmap, editor = getOption("editor")) {
    file <- tempfile(fileext = ".yaff")
    on.exit(unlink(file))

    bitmap <- bm_clamp(bitmap)
    s <- format(bitmap, px = c(".", "@"))
    cat(s, sep = "\n", file = file)

    utils::file.edit(file, editor = editor)

    as_bm_bitmap_yaff(readLines(file))
}
