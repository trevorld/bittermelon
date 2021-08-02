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

})
