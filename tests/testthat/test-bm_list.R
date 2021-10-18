font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
capital_r <- font[[str2ucp("R")]]
space <- font[[str2ucp(" ")]]

test_that("bm_list()", {

    expect_error(bm_list(2), "Some elements were not")

    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign_glyph <- bm_bitmap(plus_sign)
    plus_sign_code_point <- name2ucp("PLUS SIGN") # "U+002B" code point
    expect_equal(ncol(plus_sign_glyph), bm_widths(plus_sign_glyph))
    expect_equal(nrow(plus_sign_glyph), bm_heights(plus_sign_glyph))

    space_glyph <- bm_bitmap(matrix(0L, nrow = 9L, ncol = 9L))
    space_code_point <- name2ucp("SPACE") # "U+0020" code point

    l <- list()
    l[[plus_sign_code_point]] <- plus_sign_glyph
    l[[space_code_point]] <- space_glyph
    glyph_list <- as_bm_list(l)
    expect_true(is_bm_list(glyph_list))

    glyph_list <- bm_clamp(glyph_list)
    expect_true(is_bm_list(glyph_list))

    expect_true(is_bm_list(glyph_list[1]))

    expect_equal(glyph_list, do.call(bm_list, glyph_list))
    expect_equal(glyph_list, as_bm_list(glyph_list))

    padding_lengths <- bm_padding_lengths(glyph_list)
    expect_equal(length(padding_lengths), 2L)
    expect_equal(length(unlist(padding_lengths)), 8L)
})

test_that("as_bm_list()", {
    # Test 'as_bm_list.character()'
    bml <- as_bm_list("RSTATS", font = font)
    bml <- bm_extend(bml, sides = 1L, value = 0L)
    bml <- bm_extend(bml, sides = c(2L, 1L), value = 2L)
    bm <- bm_call(bml, cbind)

    verify_output("txt/RSTATS.txt", print(bm, px = c(" ", "#", "X")))

    bml <- !as_bm_list("RSTATS", font = font)
    bm <- do.call(cbind, bml)
    verify_output("txt/RSTATS_inverted.txt", print(bm, px = c(" ", "#", "X")))

    expect_equal(as_bm_list("", font = font), bm_list())
})

test_that("bm_widths() and bm_heights()", {
    expect_equal(bm_widths(font), 8L)
    expect_equal(bm_heights(font), 16L)

    expect_equal(length(font), length(bm_widths(font, unique = FALSE)))
    expect_equal(length(font), length(bm_heights(font, unique = FALSE)))
})

test_that("bm_padding_lengths()", {

    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign_glyph <- bm_call(plus_sign, bm_bitmap)
    bpl <- bm_padding_lengths(plus_sign_glyph)
    expect_equal(bpl[1], 2L)
    expect_equal(bpl[2], 2L)
    expect_equal(bpl[3], 2L)
    expect_equal(bpl[4], 2L)

    space_glyph <- bm_bitmap(matrix(0L, nrow = 9L, ncol = 9L))
    bpl <- bm_padding_lengths(space_glyph)
    expect_equal(bpl[1], 4L)
    expect_equal(bpl[2], 4L)
    expect_equal(bpl[3], 5L)
    expect_equal(bpl[4], 5L)

    bpl <- bm_padding_lengths(capital_r)
    expect_equal(bpl[1], 2L)
    expect_equal(bpl[2], 1L)
    expect_equal(bpl[3], 4L)
    expect_equal(bpl[4], 0L)
})

test_that("Summary.bm_list()", {
    bml <- bm_list(capital_r, space)
    expect_equal(min(!bml), 0L)
    expect_equal(max(+bml), 1L)
    expect_equal(max((4L * bml) / 2L), 2L)
    expect_equal(max(2L + bml - 2L), 1L)
    expect_equal(min(bml ^ 2L %/% 1L %% 1L & 1L | 1L == 1L), 1L)
    expect_equal(range(bml), c(0L, 1L))
    expect_error(prod(bml), "Summary function 'prod' not defined")
})

test_that("bm_overlay()", {
    grave <- font[[str2ucp("`")]]
    a <- font[[str2ucp("a")]]
    a_grave <- bm_overlay(a, over = grave)
    verify_output("txt/a_grave_overlay.txt",
        print(a_grave, px = px_ascii))
})

test_that("bm_mask()", {
    one <- font[[str2ucp("1")]]
    circle_large <- as_bm_bitmap(grid::circleGrob(r = 0.50), width = 16L, height = 16L)
    circle_small <- as_bm_bitmap(grid::circleGrob(r = 0.40), width = 16L, height = 16L)

    circle_outline <- bm_mask(circle_large, circle_small)
    verify_output("txt/circle_outline.txt",
        print(circle_outline, px = px_ascii))

    # U+2776 "Dingbat Negative Circled Digit One"
    circle_minus_one <- bm_mask(circle_large, one)
    verify_output("txt/circle_minus_one.txt",
        print(circle_minus_one, px = px_ascii))

    square_full <- bm_bitmap(matrix(1L, nrow = 16L, ncol = 16L))
    square_minus_lower_left <- square_full
    square_minus_lower_left[1:8, 1:8] <- 0L
    circle_minus_lower_left <- bm_mask(circle_large, square_minus_lower_left, mode = "alpha")
    verify_output("txt/circle_minus_lower_left.txt",
        print(circle_minus_lower_left, px = px_ascii))
})

test_that("bm_flip()", {
    verify_output("txt/capital_r_flip.txt",
        print(bm_flip(capital_r), px = px_ascii))

    verify_output("txt/capital_r_hflip.txt",
        print(bm_flip(capital_r, "h"), px = px_ascii))

    verify_output("txt/capital_r_bflip.txt",
        print(bm_flip(capital_r, "b"), px = px_ascii))

    verify_output("txt/capital_r_flip_ip.txt",
        print(bm_flip(capital_r, "v", TRUE), px = px_ascii))

    verify_output("txt/capital_r_hflip_ip.txt",
        print(bm_flip(capital_r, "h", TRUE), px = px_ascii))
})

test_that("bm_expand()", {
    verify_output("txt/capital_r_expand_2w.txt",
        print(bm_expand(capital_r, width = 2L), px = px_ascii))

    verify_output("txt/capital_r_expand_2v.txt",
        print(bm_expand(capital_r, height = 2L), px = px_ascii))

    verify_output("txt/capital_r_expand_2vw.txt",
        print(bm_expand(capital_r, width = 2L, height = 2L), px = px_ascii))

    zero <- bm_bitmap(matrix(integer(), nrow = 0, ncol = 0))
    zero_expand <- bm_expand(zero, width = 2L, height = 2L)
    expect_equal(dim(zero_expand), c(0, 0))
})

test_that("bm_compress()", {
    verify_output("txt/capital_r_compress_v.txt",
        print(bm_compress(capital_r, direction = "vertical"), px = px_ascii))

    verify_output("txt/capital_r_compress_h.txt",
        print(bm_compress(capital_r, direction = "horizontal"), px = px_ascii))

    verify_output("txt/capital_r_compress_b.txt",
        print(bm_compress(capital_r, direction = "both"), px = px_ascii))
})

test_that("as_bm_bitmap.character()", {
    verify_output("txt/abbc.txt",
        print(as_bm_bitmap("RSTATS", font = font, direction = "lr"), px = px_ascii))

    verify_output("txt/abbc_rltb.txt",
        print(as_bm_bitmap(c("RSTATS", "IS COOL!"),
                           font = font, direction = "rlbt"), px = px_ascii))

    verify_output("txt/abbc_v.txt",
        print(as_bm_bitmap("RSTATS", font = font, direction = "ttb"), px = px_ascii))

    verify_output("txt/abbc_tbrl.txt",
        print(as_bm_bitmap(c("RSTATS", "IS COOL!"),
                           font = font, direction = "tbrl"), px = px_ascii))
})
