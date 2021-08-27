#' Read and write yaff bitmap font files
#'
#' `read_yaff()` reads in yaff format bitmap font files
#' as a [bm_font()] object while `write_yaff()` writes a [bm_font()] object
#' as a yaff format bitmap font file.
#' @param con A connection object or a character string of a filename.
#'            See [base::readLines()] or [base::writeLines()] for more info.
#'            If it is a connection it will be explicitly closed.
#'
#' @param font A [bm_font()] object.
#' @examples
#'  font_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
#'  font <- read_yaff(font_file)
#'  capital_r <- font[[str2ucp("R")]]
#'  print(capital_r, px = c(".", "#"))
#'
#'  filename <- tempfile(fileext = ".yaff")
#'  write_yaff(font, filename)
#' @export
#' @rdname yaff_font
#' @return `read_yaff()` returns a [bm_font()] object.
#'         `write_yaff()` returns invisibly a character vector of the contents
#'         of the yaff font file it wrote to `con` as a side effect.
#' @seealso [bm_font()] for information about bitmap font objects.
#'     For more information about yaff font format see <https://github.com/robhagemans/monobit#the-yaff-format>.
read_yaff <- function(con) {
    if (inherits(con, "connection"))
        on.exit(close(con))

    contents <- readLines(con)

    # capture global comments (comments top of file)
    comments <- capture_comments(contents)
    contents <- grep("^#", contents, value = TRUE, invert = TRUE)

    # capture glyphs
    gl_contents <- capture_yaff_glyphs(contents)
    gl <- gl_contents$glyphs
    contents <- gl_contents$contents

    # capture properties
    properties <- capture_yaff_properties(contents)

    bm_font(gl, comments = comments, properties = properties)
}

capture_comments <- function(contents) {
    non_comments <- which(grepl("^[^#]|^$", contents))
    if (length(non_comments)) {
        first_non_comment <- min(non_comments)
        if (first_non_comment > 1L) {
            comments <- contents[seq.int(first_non_comment - 1L)]
        } else {
            comments <- character()
        }
    } else {
        comments <- grep("^#", contents)
    }
    comments <- gsub("# {0,1}", "", comments)
    if (length(comments) == 0L)
        comments <- NULL
    comments
}

capture_yaff_properties <- function(contents) {
    one_liner_token <- "^[[:alnum:]_.-]+:[[:space:]]*[^[:space:]]+"
    i_one_liners <- grep(one_liner_token, contents)
    one_liners <- contents[i_one_liners]

    key_token <- "(^[[:alnum:]_.-]+):([[:space:]]*)(.*)"
    keys <- gsub(key_token, "\\1", one_liners)
    values <- gsub(key_token, "\\3", one_liners)
    values <- gsub("[[:space:]]*$", "", values) # strip trailing whitespace
    properties <- as.list(values)
    names(properties) <- keys

    # Support multi-line properties
    multi_line <- contents[-i_one_liners]
    multi_line <- grep("^$", multi_line, value = TRUE, invert = TRUE)
    i_keys <- grep(".*:$", multi_line)
    keys <- gsub(":$", "", multi_line[i_keys])
    i_values_start <- i_keys + 1L
    i_values_end <- c(i_keys[2:length(i_keys)] + 1L, length(multi_line))
    for (i in seq_along(keys)) {
        value <- multi_line[i_values_start[i]:i_values_end[i]]
        value <- gsub("^[[:space:]]+", "", value)
        value <- gsub("[[:space:]]+$", "", value)
        properties[[keys[i]]] <- value
    }

    properties
}

last <- function(v) v[length(v)]

capture_yaff_glyphs <- function(contents) {
    glyph_token <- "^[[:space:]]+(-{1}|[@\\.]+)[[:space:]]*$"
    indices_glyphs <- grep(glyph_token, contents)
    if (length(indices_glyphs) == 0) {
        return(list(glyphs = bm_list(), contents = contents))
    }
    indices_indices_last <- which(diff(indices_glyphs) > 1L)
    indices_first <- c(indices_glyphs[1L], indices_glyphs[indices_indices_last + 1L])
    indices_last <- c(indices_glyphs[indices_indices_last], last(indices_glyphs))

    gl <- bm_list()
    for (i in seq_along(indices_first)) {
        first <- indices_first[i]
        last <- indices_last[i]
        glyph <- as_bm_bitmap_yaff(contents[first:last])
        labels <- get_yaff_labels(contents, first)
        indices_glyphs <- append(indices_glyphs, seq.int(first - length(labels) - 1L, first - 1L))
        ucp <- label2ucp(labels)
        gl[[ucp]] <- glyph
    }
    list(glyphs = gl, contents = contents[-indices_glyphs])
}

get_yaff_labels <- function(contents, starting_index) {
    label_token <- "^[[:alnum:]+_ <>-]+:$"
    labels <- c()
    j <- starting_index - 1L
    is_label <- TRUE
    while (is_label && j > 0L) {
        if (grepl(label_token, contents[j])) {
            labels <- append(labels, contents[j])
            j <- j - 1L
        } else {
            is_label <- FALSE
        }
    }
    labels <- gsub(":$", "", labels)
    labels
}

label2ucp <- function(labels) {
        labels <- yaff_ucp_sort(labels)
        ucp <- sapply(labels, yaff2ucp_helper)
        ucp <- Filter(Negate(is.na), ucp)[1]
        if (is.na(ucp))
            stop(paste("Couldn't determine Unicode code point from labels:", labels))
        ucp
}

yaff_ucp_sort <- function(x) {
    uplus <- base::which(toupper(substr(x, 1, 2)) == "U+")
    if (length(uplus))
        c(x[uplus], x[-uplus])
    else
        x
}

yaff2ucp_helper <- function(label) {
    if (toupper(substr(label, 1, 2)) == "U+") {
        hex2ucp(toupper(label))
    } else if (substr(label, 1, 2) == "0x") {
        hex2ucp(label)
    } else if (substr(label, 1, 2) == "0o") {
        int2ucp(substr(label, 1, 2))
    } else if (grepl("^[[:digit:]]+", label)) {
        int2ucp(label)
    } else {
        name2ucp(toupper(label))
    }
}

as_bm_bitmap_yaff <- function(glyph) {
    glyph <- rev(glyph)
    glyph <- gsub("[[:space:]]", "", glyph)

    if (length(glyph) == 1 && glyph == "-")
        return(bm_bitmap(matrix(0L, nrow = 0L, ncol = 0L)))

    glyph <- gsub("\\.", "0", glyph)
    glyph <- gsub("@", "1", glyph)
    binary <- as.integer(strsplit(paste(glyph, collapse = ""), "")[[1]])
    nr <- length(glyph)
    nc <- nchar(glyph[1])
    m <- matrix(binary, nrow = nr, ncol = nc, byrow = TRUE)
    bm_bitmap(m)
}

#' @rdname yaff_font
#' @export
write_yaff <- function(font, con = stdout()) {
    if (inherits(con, "connection"))
        on.exit(close(con))

    validate_bm_font(font)
    # yaff fonts only support black-and-white glyphs
    if (any(sapply(font, function(x) max(x) > 1L))) {
        message("Multi-colored glyphs detected, casting to black-and-white.")
        font <- bm_clamp(font)
    }

    contents <- character()

    # global comments
    comments <-  attr(font, "comments")
    if (length(comments)) {
        comments <- paste0("# ", comments)
        contents <- c(contents, c(comments, ""))
    }

    # properties
    properties <- attr(font, "properties")
    if (length(properties)) {
        keys <- names(properties)
        properties <- lapply(seq_along(keys),
                             function(i) as_yaff_property(keys[i], properties[[i]]))
        properties <- unlist(properties, use.names = FALSE)
        contents <- c(contents, properties, "")
    }

    # glyphs
    code_points <- names(font)
    glyphs <- unlist(lapply(code_points, as_yaff_bm_bitmap, font),
                     use.names = FALSE)
    contents <- append(contents, glyphs)

    writeLines(contents, con)
    invisible(contents)
}

as_yaff_property <- function(key, value) {
    if (length(value) == 1) {
        paste0(key, ": ", value)
    } else {
        c(paste0(key, ":"),
          paste0("    ", value))
    }
}

as_yaff_bm_bitmap <- function(code_point, font) {
    glyph <- font[[code_point]]
    glyph <- bm_extend(glyph, left = 4L, value = 2L)
    label <- ucp2label(code_point)
    c(paste0(code_point, ":"),
      paste0(label, ":"),
      format(glyph, px = c(".", "@", " ")),
      "")
}
