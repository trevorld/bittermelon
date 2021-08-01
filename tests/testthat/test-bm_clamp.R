test_that("bm_clamp()", {
    skip_on_os("windows")
    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 2L
    plus_sign[3:7, 5L] <- 2L
    plus_sign_glyph <- bm_bitmap(plus_sign)
    expect_equal(min(plus_sign_glyph), 0L)
    expect_equal(max(plus_sign_glyph), 2L)

    plus_sign_clamped <- bm_clamp(plus_sign_glyph)
    expect_equal(min(plus_sign_clamped), 0L)
    expect_equal(max(plus_sign_clamped), 1L)
    expect_equal(sum(plus_sign_clamped == 0L), 72L)
    expect_equal(sum(plus_sign_clamped == 1L), 9L)

    plus_sign_clamped <- bm_clamp(plus_sign_glyph, value = 0L)
    expect_equal(min(plus_sign_clamped), 0L)
    expect_equal(max(plus_sign_clamped), 0L)
    expect_equal(sum(plus_sign_clamped == 0L), 81L)
    expect_equal(sum(plus_sign_clamped == 1L), 0L)

    plus_sign_clamped <- bm_clamp(plus_sign_glyph, lower = 1L, upper = 2L)
    expect_equal(min(plus_sign_clamped), 1L)
    expect_equal(max(plus_sign_clamped), 2L)
    expect_equal(sum(plus_sign_clamped == 1L), 72L)
    expect_equal(sum(plus_sign_clamped == 2L), 9L)

    plus_sign_clamped <- bm_clamp(plus_sign_glyph, lower = 1L, upper = 2L, value = c(2L, 2L))
    expect_equal(min(plus_sign_clamped), 2L)
    expect_equal(max(plus_sign_clamped), 2L)
    expect_equal(sum(plus_sign_clamped == 1L), 0L)
    expect_equal(sum(plus_sign_clamped == 2L), 81L)
})
