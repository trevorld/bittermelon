plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
plus_sign[5L, 3:7] <- 1L
plus_sign[3:7, 5L] <- 1L
plus_sign_glyph <- bm_bitmap(plus_sign)
plus_sign_code_point <- str2ucp("+") # code point U+002B

space_glyph <- bm_bitmap(matrix(0L, nrow = 9L, ncol = 9L))
space_code_point <- name2ucp("SPACE") # code point U+0020

test_that("bm_font()", {
	expect_error(bm_font(2), "Some elements were not")

	l <- list()
	l[[plus_sign_code_point]] <- plus_sign_glyph
	l[[space_code_point]] <- space_glyph
	font <- as_bm_font(l)
	expect_true(is_bm_font(font))
	expect_equal(font, bm_font(font))
	expect_equal(font, as_bm_font(font))

	l <- list()
	class(l) <- "bm_font"
	expect_snapshot(validate_bm_font(l), error = TRUE)
	l$foo <- "bananaslug"
	expect_snapshot(validate_bm_font(l), error = TRUE)
})

test_that("summary.bm_font()", {
	l <- list()
	l[[plus_sign_code_point]] <- plus_sign_glyph
	l[[space_code_point]] <- space_glyph
	font <- as_bm_font(l)

	s <- summary(font)
	expect_s3_class(s, "summary_bm_font")
	expect_null(s$name)
	expect_equal(s$n_chars, 2L)
	expect_equal(s$coverage$block, "Basic Latin")
	expect_equal(s$coverage$n, 2L)

	expect_snapshot(print(s))
})

test_that("summary.bm_font() snapshot with empty font", {
	expect_snapshot(print(summary(bm_font())))
})

test_that("summary.bm_font() snapshot with fixed 4x6 font", {
	font <- read_yaff(system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon"))
	expect_snapshot(print(summary(font)))
})
