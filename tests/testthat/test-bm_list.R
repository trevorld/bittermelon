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
    glyph_list <- as_bm_list(l)
    expect_true(is_bm_list(glyph_list))

    glyph_list <- bm_clamp(glyph_list)
    expect_true(is_bm_list(glyph_list))

    expect_equal(glyph_list, do.call(bm_list, glyph_list))
    expect_equal(glyph_list, as_bm_list(glyph_list))
})

test_that("as_bm_list()", {
    # Test 'as_bm_list.character()'
    font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    font <- read_hex(font_file)
    bml <- as_bm_list("RSTATS", font = font)
    bml <- bm_extend(bml, sides = 1L, value = 0L)
    bml <- bm_extend(bml, sides = c(2L, 1L), value = 2L)
    bm <- do.call(cbind, bml)

    verify_output("txt/RSTATS.txt", print(bm, labels = c(" ", "#", "X")))

    expect_equal(bm_widths(font), 8L)
    expect_equal(bm_heights(font), 16L)

    expect_equal(length(font), length(bm_widths(font, unique = FALSE)))
    expect_equal(length(font), length(bm_heights(font, unique = FALSE)))
})
