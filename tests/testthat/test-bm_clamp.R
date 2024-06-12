test_that("bm_clamp()", {
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

    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    corn <- farming_crops_16x16()$corn$portrait
    corn_l <- bm_list(as_bm_bitmap(corn))
    corn_r <- as.raster(corn)
    corn_nr <- as.raster(corn, native = TRUE)
    corn_mi <- magick::image_read(corn)
    corn_c <- bm_clamp(corn, "cyan")
    corn_lc <- bm_clamp(corn_l)
    corn_rc <- bm_clamp(corn_r, "green")
    corn_nrc <- bm_clamp(corn_nr, "magenta")
    corn_mic <- bm_clamp(corn_mi, "red")
    verify_output("txt/bm_clamp.txt", {
        print(corn_c, compress = "v")
        print(corn_lc)
        bm_print(corn_rc, compress = "v")
        bm_print(corn_nrc, compress = "v")
        bm_print(corn_mic, compress = "v")
    }, unicode = TRUE, crayon = TRUE)
})
