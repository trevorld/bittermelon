test_that("bm_bytepad()", {
	bm <- bm_bitmap(matrix(1L, nrow = 6L, ncol = 9L))

	# pads right to next multiple of 8
	result <- bm_bytepad(bm)
	expect_equal(ncol(result), 16L)
	expect_equal(nrow(result), 6L)

	# padding pixels are 0
	expect_equal(result[, 10:16], matrix(0L, nrow = 6L, ncol = 7L))

	# already a multiple of 8 — no change
	bm8 <- bm_bitmap(matrix(1L, nrow = 6L, ncol = 8L))
	expect_equal(ncol(bm_bytepad(bm8)), 8L)

	# works on bm_list
	font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
	font <- read_hex(font_file)
	capital_r <- font[[str2ucp("R")]]
	capital_r_7 <- bm_trim(capital_r, right = 1L)
	expect_equal(ncol(capital_r_7), 7L)
	expect_equal(ncol(bm_bytepad(capital_r_7)), 8L)
})
