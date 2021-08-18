test_that("print.bm_bitmap()", {
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

    skip_on_os("windows")
    verify_output("txt/plus_unicode.txt", {
        print(bm_bitmap(plus_sign))
    })

    plus_space_bt <- rbind(plus_sign, space_glyph, direction = "bottom-to-top")
    verify_output("txt/plus_space_bt.txt", {
        print(bm_bitmap(plus_space_bt))
    })

    plus_space_rl <- cbind(plus_sign, space_glyph, direction = "right-to-left")
    verify_output("txt/plus_space_rl.txt", {
        print(bm_bitmap(plus_space_rl))
    })

    skip_if_not_installed("crayon")
    verify_output("txt/plus_unicode_color_char.txt", {
        print(bm_bitmap(plus_sign), fg = "blue", bg = "red")
    }, crayon = TRUE)

    verify_output("txt/plus_unicode_color_options.txt", {
        withr::with_options(list(bittermelon.px = px_ascii,
                                 bittermelon.compress = "v",
                                 bittermelon.fg = "blue",
                                 bittermelon.bg = "white"),
                            print(bm_bitmap(plus_sign)))
        }, crayon = TRUE)

    expect_length(format(bm_bitmap(matrix(0L, nrow = 0L, ncol = 0L))), 0L)
})
