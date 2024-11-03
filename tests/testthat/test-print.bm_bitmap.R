test_that("print.bm_bitmap()", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign <- bm_bitmap(plus_sign)
    verify_output("txt/plus.txt", {
        print(bm_bitmap(plus_sign), px = c(".", "#"))
    })

    space_glyph <- bm_bitmap(matrix(0L, nrow = 9L, ncol = 9L))
    verify_output("txt/space.txt", {
        print(space_glyph, px = c("."))
    })

    expect_length(format(bm_bitmap(matrix(character(), nrow = 0L, ncol = 0L))), 0L)
    expect_length(format(bm_bitmap(matrix(character(), nrow = 0L, ncol = 4L))), 0L)
    expect_length(format(bm_bitmap(matrix(character(), nrow = 4L, ncol = 0L))), 0L)

    skip_if(!cli::is_utf8_output())
    verify_output("txt/plus_unicode.txt", {
        print(bm_bitmap(plus_sign), downscale = TRUE)
    }, unicode = TRUE)

    plus_space_bt <- rbind(plus_sign, space_glyph, direction = "bottom-to-top")
    verify_output("txt/plus_space_bt.txt", {
        print(bm_bitmap(plus_space_bt))
    }, unicode = TRUE)

    plus_space_rl <- cbind(plus_sign, space_glyph, direction = "right-to-left")
    verify_output("txt/plus_space_rl.txt", {
        print(bm_bitmap(plus_space_rl), fg = cli::col_none, bg = cli::bg_none)
    }, unicode = TRUE)

    verify_output("txt/plus_unicode_color_char.txt", {
        print(bm_bitmap(plus_sign), fg = "blue", bg = "red")
    }, crayon = TRUE, unicode = TRUE)

    verify_output("txt/plus_unicode_color_options.txt", {
        withr::with_options(list(bittermelon.px = px_ascii,
                                 bittermelon.compress = "v",
                                 bittermelon.fg = "blue",
                                 bittermelon.bg = "white"),
                            print(bm_bitmap(plus_sign)))
        }, crayon = TRUE, unicode = TRUE)
})
