test_that("print.bm_bitmap()", {
    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign <- bm_bitmap(plus_sign)
    verify_output("txt/plus.txt", {
        print(bm_bitmap(plus_sign), labels = c(".", "#"))
    })

    space_glyph <- bm_bitmap(matrix(0L, nrow = 8L, ncol = 8L))
    verify_output("txt/space.txt", {
        print(space_glyph, labels = c("."))
    })

    skip_on_os("windows")
    verify_output("txt/plus_unicode.txt", {
        print(bm_bitmap(plus_sign))
    })

})
