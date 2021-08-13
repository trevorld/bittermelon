test_that("bm_bitmap()", {

    expect_error(bm_bitmap("Zippity"))
    expect_error(bm_bitmap(matrix("Zippity")))

    space_matrix <- matrix(0L, nrow = 16, ncol = 16)
    space_glyph <- bm_bitmap(space_matrix)
    expect_true(is_bm_bitmap(space_glyph))
    expect_false(is_bm_bitmap(space_matrix))
    expect_equal(nrow(space_glyph), 16)
    expect_equal(ncol(space_glyph), 16)

    space_glyph2 <- bm_bitmap(space_glyph)
    expect_equal(space_glyph, space_glyph2)

    space_glyph3 <- as_bm_bitmap(space_matrix)
    expect_equal(space_glyph, space_glyph3)

    space_matrix2 <- as.matrix(space_glyph)
    expect_equal(space_matrix, space_matrix2)
})

test_that("as_bm_bitmap()", {
    skip_if_not(capabilities("png"))
    circle <- as_bm_bitmap(grid::circleGrob(r = 0.25), width = 16L, height = 16L)
    verify_output("txt/circle_grob.txt", print(circle, px = c(".", "@")))

    circle_outline <- bm_outline(circle)
    verify_output("txt/circle_bm_outline.txt",
                  print(circle_outline, px = px_ascii))
})
