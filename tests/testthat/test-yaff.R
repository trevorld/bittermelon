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

test_that("is_combining_character()", {
	expect_false(is_combining_character(str2ucp("a")))
	expect_true(is_combining_character("U+0300"))
	expect_true(is_combining_character("U+20DD"))
})
