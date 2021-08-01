test_that("bm_extend()", {
    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign <- bm_bitmap(plus_sign)
    expect_true(is_bm_bitmap(plus_sign))
    expect_equal(nrow(plus_sign), 9L)
    expect_equal(ncol(plus_sign), 9L)

    plus_sign_p1 <- bm_extend(plus_sign, sides = 1L)
    expect_true(is_bm_bitmap(plus_sign_p1))
    expect_equal(nrow(plus_sign_p1), 11L)
    expect_equal(ncol(plus_sign_p1), 11L)

    plus_sign_p12 <- bm_extend(plus_sign, sides = 1:2)
    expect_equal(nrow(plus_sign_p12), 11L)
    expect_equal(ncol(plus_sign_p12), 13L)

    plus_sign_p123 <- bm_extend(plus_sign, sides = 1:3)
    expect_equal(nrow(plus_sign_p123), 13L)
    expect_equal(ncol(plus_sign_p123), 13L)

    plus_sign_p1234 <- bm_extend(plus_sign, sides = 1:4)
    expect_equal(nrow(plus_sign_p1234), 13L)
    expect_equal(ncol(plus_sign_p1234), 15L)

    expect_equal(nrow(bm_extend(plus_sign, top = 2L)), 11L)
    expect_equal(ncol(bm_extend(plus_sign, top = 2L)), 9L)
    expect_equal(nrow(bm_extend(plus_sign, right = 2L)), 9L)
    expect_equal(ncol(bm_extend(plus_sign, right = 2L)), 11L)
    expect_equal(nrow(bm_extend(plus_sign, bottom = 2L)), 11L)
    expect_equal(ncol(bm_extend(plus_sign, bottom = 2L)), 9L)
    expect_equal(nrow(bm_extend(plus_sign, left = 2L)), 9L)
    expect_equal(ncol(bm_extend(plus_sign, left = 2L)), 11L)

    skip_on_os("windows")
    verify_output("txt/plus_sign_left_and_width.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, left = 2L))
    verify_output("txt/plus_sign_right_and_width.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, right = 2L))
    verify_output("txt/plus_sign_left.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "left"))
    verify_output("txt/plus_sign_center_left.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "centre-left"))
    verify_output("txt/plus_sign_center_right.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "centre-right"))
    verify_output("txt/plus_sign_right.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "right"))
    verify_output("txt/plus_sign_top.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "top"))
    verify_output("txt/plus_sign_top_and_height.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, top = 2L))
    verify_output("txt/plus_sign_bottom_and_height.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, bottom = 2L))
    verify_output("txt/plus_sign_center_top.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "centre-top"))
    verify_output("txt/plus_sign_center_bottom.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "centre-bottom"))
    verify_output("txt/plus_sign_bottom.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "bottom"))
})
