#' Bitmap font object
#'
#' `bm_font()` creates a bitmap font object.
#'
#' `bm_font()` is a named list.
#' The names are of the form \dQuote{U+HHHH} or \dQuote{U+HHHHH}.
#' where the `H` are appropriate hexadecimal Unicode code points.
#' It is a subclass of [bm_list()].
#' @param x Named list of [bm_bitmap()] objects.
#'          Names must be coercible by [Unicode::as.u_char()].
#' @param comments An optional character vector of (global) font comments.
#' @param properties An optional named list of font metadata.
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  is_bm_font(font)
#'
#'  # number of characters in font
#'  length(font)
#'
#'  # print out "R"
#'  R_glyph <- font[[str2ucp("R")]]
#'  print(R_glyph)
#' @return A named list with a \dQuote{bm_font} subclass.
#' @seealso [is_bm_font()], [as_bm_font()], [hex2ucp()]
#' @export
bm_font <- function(x = bm_list(), comments = NULL, properties = NULL) {
	if (is_bm_font(x)) {
		x
	} else {
		as_bm_font(x, comments = comments, properties = properties)
	}
}

#' Test if the object is a bitmap font object
#'
#' `is_bm_font()` returns `TRUE` for `bm_font` objects (or subclasses)
#' and `FALSE` for all other objects.
#' @param x An object
#' @return `TRUE` or `FALSE`
#' @examples
#'  font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
#'  font <- read_hex(font_file)
#'  is_bm_font(font)
#' @seealso [bm_font()]
#' @export
is_bm_font <- function(x) {
	inherits(x, "bm_font")
}

#' Coerce to bitmap font objects
#'
#' `as_bm_font()` turns an existing object into a `bm_font()` object.
#'
#' @param x An object that can reasonably be coerced to a `bm_font()` object.
#' @param ... Further arguments passed to or from other methods.
#' @inheritParams bm_font
#' @return A `bm_font()` object.
#' @examples
#'   plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
#'   plus_sign[5L, 3:7] <- 1L
#'   plus_sign[3:7, 5L] <- 1L
#'   plus_sign_glyph <- bm_bitmap(plus_sign)
#'
#'   space_glyph <- bm_bitmap(matrix(0L, nrow = 9L, ncol = 9L))
#'
#'   l <- list()
#'   l[[str2ucp("+")]] <- plus_sign_glyph
#'   l[[str2ucp(" ")]] <- space_glyph
#'   font <- as_bm_font(l)
#'   is_bm_font(font)
#'
#' @seealso [bm_font()]
#' @export
as_bm_font <- function(x, ..., comments = NULL, properties = NULL) {
	UseMethod("as_bm_font")
}

#' @rdname as_bm_font
#' @export
as_bm_font.default <- function(x, ..., comments = NULL, properties = NULL) {
	as_bm_font.list(as.list(x), comments = comments, properties = properties)
}

#' @rdname as_bm_font
#' @export
as_bm_font.list <- function(x, ..., comments = NULL, properties = NULL) {
	if (is_bm_font(x)) {
		return(x)
	}
	x <- as_bm_list(x)
	if (length(x) == 0L) {
		names(x) <- character(0)
	}
	validate_bm_font(x)
	names(x) <- hex2ucp(names(x))
	attr(x, "comments") <- comments
	attr(x, "properties") <- properties
	class(x) <- c("bm_font", class(x))
	x
}

#' Summarize a bitmap font
#'
#' `summary.bm_font()` summarizes a [bm_font()] object, showing
#' the font name (if available), the number of characters,
#' and coverage of Unicode blocks.
#'
#' @param object A [bm_font()] object.
#' @param ... Currently ignored.
#' @return A `summary_bm_font` object (a list) with the following components:
#'   \describe{
#'     \item{name}{Font name string, or `NULL` if not available.}
#'     \item{n_chars}{Number of characters in the font.}
#'     \item{coverage}{A data frame with columns `block` (Unicode block name),
#'       `n` (number of font characters in that block), and `total`
#'       (total number of named code points in that block).
#'       Rows are sorted in Unicode code point order and include only
#'       blocks with at least one character in the font.}
#'   }
#' @examples
#'   font <- read_yaff(system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon"))
#'   summary(font)
#' @seealso [bm_font()]
#' @export
summary.bm_font <- function(object, ...) {
	properties <- attr(object, "properties")
	name <- properties[["name"]] %||% NULL

	ucps <- names(object)
	n_chars <- length(ucps)

	if (n_chars > 0L) {
		# Only count named code points for block coverage
		ucp_names <- Unicode::u_char_name(ucps)
		named_ucps <- ucps[!is.na(ucp_names) & nzchar(ucp_names)]
		block_of <- Unicode::u_char_property(named_ucps, "Block")
		block_table <- table(block_of)
		covered_block_names <- names(block_table)

		all_blocks <- Unicode::u_blocks()
		block_starts <- vapply(all_blocks, function(b) as.integer(b[[1L]])[1L], integer(1L))
		ordered_names <- names(sort(block_starts))
		ordered_covered <- ordered_names[ordered_names %in% covered_block_names]

		block_totals <- vapply(ordered_covered, function(b) length(block2ucp(b)), integer(1L))

		coverage <- data.frame(
			block = ordered_covered,
			n = as.integer(block_table[ordered_covered]),
			total = block_totals,
			row.names = NULL,
			stringsAsFactors = FALSE
		)
	} else {
		coverage <- data.frame(
			block = character(0L),
			n = integer(0L),
			total = integer(0L),
			stringsAsFactors = FALSE
		)
	}

	structure(
		list(name = name, n_chars = n_chars, coverage = coverage),
		class = "summary_bm_font"
	)
}

#' @rdname summary.bm_font
#' @param x A `summary_bm_font` object.
#' @export
print.summary_bm_font <- function(x, ...) {
	if (!is.null(x$name)) {
		cat("Bitmap font:", x$name, "\n")
	} else {
		cat("Bitmap font\n")
	}
	cat("Total characters:", x$n_chars, "\n")

	if (nrow(x$coverage) > 0L) {
		cat("Unicode block coverage (n / named chars in block):\n")
		max_block_len <- max(nchar(x$coverage$block))
		max_n <- max(x$coverage$n)
		max_total <- max(x$coverage$total)
		n_width <- nchar(max_n)
		total_width <- nchar(max_total)
		for (i in seq_len(nrow(x$coverage))) {
			row <- x$coverage[i, ]
			cat(sprintf(
				"  %-*s  %*d/%*d\n",
				max_block_len,
				row$block,
				n_width,
				row$n,
				total_width,
				row$total
			))
		}
	}
	invisible(x)
}

validate_bm_font <- function(x) {
	if (is.null(names(x)) || any(names(x) == "")) {
		stop("'x' must be a **named** list (with Unicode code point names)")
	}
	codepoints <- hex2ucp(names(x))
	if (any(is.na(codepoints))) {
		stop("Some names were not coercible by `Unicode::as_u_char()`")
	}
	invisible(NULL)
}
