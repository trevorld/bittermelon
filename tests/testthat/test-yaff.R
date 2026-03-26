test_that("read_yaff() and write_yaff()", {
	yaff_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
	yaff <- read_yaff(yaff_file)
	expect_true(is_bm_font(yaff))

	expect_true(any(grepl("Send bug reports to Markus Kuhn", attr(yaff, "comments"))))

	expect_equal(attr(yaff, "properties")$name, "Fixed Medium 6")
	expect_equal(attr(yaff, "properties")[["source-name"]], "4x6.bdf")

	plus_cp <- name2ucp("PLUS SIGN") # code point U+002B
	expect_equal(length(read_yaff(textConnection(""))), 0L)

	f <- tempfile(fileext = ".yaff.gz")
	write_yaff(yaff, gzfile(f))
	yaff2 <- read_yaff(f)
	unlink(f)

	expect_equal(yaff[[plus_cp]], yaff2[[plus_cp]])

	pixmap_font <- bm_lapply(yaff[plus_cp], as_bm_pixmap)
	expect_snapshot(write_yaff(pixmap_font, tempfile()), error = TRUE)

	multi <- yaff
	multi[[plus_cp]] <- 2L * yaff[[plus_cp]]
	expect_snapshot(write_yaff(multi, tempfile()), error = TRUE)
})

test_that("read_yaff() greyscale levels", {
	plus_cp <- name2ucp("PLUS SIGN")

	yaff4 <- read_yaff(textConnection(c(
		"levels: 4",
		"",
		"u+002B:",
		"    .@.",
		"    121",
		"    @@@"
	)))
	expect_true(is_bm_font(yaff4))
	expect_true(is_bm_list(yaff4, class = "bm_pixmap"))
	g4 <- yaff4[[plus_cp]]
	expect_equal(g4[3L, 1L], "#FFFFFF00") # . (top row)
	expect_equal(g4[3L, 2L], "#000000FF") # @ (top row)
	expect_equal(g4[2L, 1L], sprintf("#000000%02X", as.integer(round(1 / 3 * 255)))) # 1 = lighter grey (middle row)
	expect_equal(g4[2L, 2L], sprintf("#000000%02X", as.integer(round(2 / 3 * 255)))) # 2 = darker grey (middle row)

	yaff16 <- read_yaff(textConnection(c(
		"levels: 16",
		"",
		"u+002B:",
		"    .5@",
		"    AAA"
	)))
	expect_true(is_bm_font(yaff16))
	expect_true(is_bm_list(yaff16, class = "bm_pixmap"))
	g16 <- yaff16[[plus_cp]]
	expect_equal(g16[2L, 1L], "#FFFFFF00") # . = transparent
	expect_equal(g16[2L, 3L], "#000000FF") # @ = black
	expect_equal(g16[2L, 2L], sprintf("#000000%02X", as.integer(round(5 / 15 * 255)))) # 5

	yaff256 <- read_yaff(textConnection(c(
		"levels: 256",
		"",
		"u+002B:",
		"    ..80@@",
		"    AAAAAA"
	)))
	expect_true(is_bm_font(yaff256))
	expect_true(is_bm_list(yaff256, class = "bm_pixmap"))
	g256 <- yaff256[[plus_cp]]
	expect_equal(g256[2L, 1L], "#FFFFFF00") # .. = transparent
	expect_equal(g256[2L, 3L], "#000000FF") # @@ = black
	expect_equal(g256[2L, 2L], "#00000080") # 80 = hex 0x80
})

test_that("read_yaff() all-comment file (no blank lines)", {
	font <- read_yaff(textConnection(c("# just a comment", "# another comment")))
	expect_equal(length(font), 0L)
	expect_equal(attr(font, "comments"), c("just a comment", "another comment"))
})

test_that("read_yaff() multi-line property", {
	plus_cp <- name2ucp("PLUS SIGN")
	font <- read_yaff(textConnection(c(
		"name: MyFont",
		"notice:",
		"    line 1",
		"    line 2",
		"",
		"u+002B:",
		"    .@."
	)))
	expect_equal(attr(font, "properties")$name, "MyFont")
	expect_equal(attr(font, "properties")$notice, "line 1\nline 2")
	expect_equal(nrow(font[[plus_cp]]), 1L)
})

test_that("read_yaff() alternative label formats", {
	plus_cp <- name2ucp("PLUS SIGN")

	font_0x <- read_yaff(textConnection(c("0x002B:", "    .@.")))
	expect_equal(nrow(font_0x[[plus_cp]]), 1L)

	font_0o <- read_yaff(textConnection(c("0o53:", "    .@.")))
	expect_equal(nrow(font_0o[[plus_cp]]), 1L)

	font_dec <- read_yaff(textConnection(c("43:", "    .@.")))
	expect_equal(nrow(font_dec[[plus_cp]]), 1L)
})

test_that("read_yaff() unrecognizable label errors", {
	expect_snapshot(
		read_yaff(textConnection(c("U+ZZZZZ:", "    .@."))),
		error = TRUE
	)
})

test_that("read_yaff() empty glyph (-)", {
	plus_cp <- name2ucp("PLUS SIGN")

	font_bm <- read_yaff(textConnection(c("u+002B:", "    -")))
	expect_equal(nrow(font_bm[[plus_cp]]), 0L)
	expect_equal(ncol(font_bm[[plus_cp]]), 0L)

	font_pm <- read_yaff(textConnection(c("levels: 4", "", "u+002B:", "    -")))
	expect_equal(nrow(font_pm[[plus_cp]]), 0L)
	expect_equal(ncol(font_pm[[plus_cp]]), 0L)
})

test_that("write_yaff() multi-line property", {
	font <- bm_font(list(), properties = list(notice = "line 1\nline 2"))
	lines <- write_yaff(font, tempfile())
	expect_true(any(grepl("^notice:$", lines)))
	expect_true(any(grepl("^    line 1$", lines)))
})

test_that("is_combining_character()", {
	expect_false(is_combining_character(str2ucp("a")))
	expect_true(is_combining_character("U+0300"))
	expect_true(is_combining_character("U+20DD"))
})
