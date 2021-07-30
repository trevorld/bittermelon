test_that("bm_list()", {

    expect_error(bm_list(2), "Some elements were not")

    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign_glyph <- bm_bitmap(plus_sign)
    plus_sign_code_point <- name2ucp("PLUS SIGN") # "U+002B" code point

    space_glyph <- bm_bitmap(matrix(0L, nrow = 9L, ncol = 9L))
    space_code_point <- name2ucp("SPACE") # "U+0020" code point

    l <- list()
    l[[plus_sign_code_point]] <- plus_sign_glyph
    l[[space_code_point]] <- space_glyph
    glyph_list <- bm_list(l)
    expect_true(is_bm_list(glyph_list))

    glyph_list <- bm_clamp(glyph_list)
    expect_true(is_bm_list(glyph_list))

    expect_equal(glyph_list, bm_list(glyph_list))
    expect_equal(glyph_list, as_bm_list(glyph_list))
})
