test_that("bm_glyph()", {

    expect_error(bm_glyph("Zippity"))
    expect_error(bm_glyph(matrix("Zippity")))

    space_matrix <- matrix(0, nrow = 16, ncol = 16)
    space_glyph <- bm_glyph(space_matrix)
    expect_true(is_bm_glyph(space_glyph))
    expect_false(is_bm_glyph(space_matrix))
    expect_equal(nrow(space_glyph), 16)
    expect_equal(ncol(space_glyph), 16)

    space_glyph2 <- bm_glyph(space_glyph)
    expect_equal(space_glyph, space_glyph2)

    space_glyph3 <- as_bm_glyph(space_matrix)
    expect_equal(space_glyph, space_glyph3)
})
