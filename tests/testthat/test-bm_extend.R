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

    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    verify_output("txt/plus_sign_left_and_width.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, left = 2L),
                  unicode = TRUE)
    verify_output("txt/plus_sign_right_and_width.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, right = 2L),
                  unicode = TRUE)
    verify_output("txt/plus_sign_left.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "left"),
                  unicode = TRUE)
    verify_output("txt/plus_sign_center_left.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "centre-left"),
                  unicode = TRUE)
    verify_output("txt/plus_sign_center_right.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "centre-right"),
                  unicode = TRUE)
    verify_output("txt/plus_sign_right.txt",
                  bm_extend(plus_sign, value = 2L, width = 12L, hjust = "right"),
                  unicode = TRUE)
    verify_output("txt/plus_sign_top.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "top"),
                  unicode = TRUE)
    verify_output("txt/plus_sign_top_and_height.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, top = 2L),
                  unicode = TRUE)
    verify_output("txt/plus_sign_bottom_and_height.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, bottom = 2L),
                  unicode = TRUE)
    verify_output("txt/plus_sign_center_top.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "centre-top"),
                  unicode = TRUE)
    verify_output("txt/plus_sign_center_bottom.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "centre-bottom"),
                  unicode = TRUE)
    verify_output("txt/plus_sign_bottom.txt",
                  bm_extend(plus_sign, value = 2L, height = 12L, vjust = "bottom"),
                  unicode = TRUE)
})

test_that("`bm_extend.bm_pixmap()`", {
    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    verify_output("txt/bm_extend_pixmap.txt", {
                  m <- matrix(c("blue", "yellow"), nrow = 2L, ncol = 2L)
                  pm <- as_bm_pixmap.matrix(m)
                  print(bm_extend(pm, value = "red", top = 1L))
                  print(bm_extend(pm, value = "red", right = 1L))
                  print(bm_extend(pm, value = "red", bottom = 1L))
                  print(bm_extend(pm, value = "red", left = 1L))
                  }, crayon = TRUE, unicode = TRUE)

})

test_that("`bm_extend.image-magick()`", {
    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    verify_output("txt/bm_extend_magick.txt", {
                  m <- matrix(c("blue", "yellow"), nrow = 2L, ncol = 2L)
                  r <- magick::image_read(as_bm_pixmap(m))
                  rt <- bm_extend(r, value = "red", top = 1L)
                  print(inherits(rt, "magick-image"))
                  print(`as_bm_pixmap.magick-image`(rt))
                  rr <- bm_extend(r, value = "red", right = 1L)
                  print(`as_bm_pixmap.magick-image`(rr))
                  rb <- bm_extend(r, value = "red", bottom = 1L)
                  print(`as_bm_pixmap.magick-image`(rb))
                  rl <- bm_extend(r, value = "red", left = 1L)
                  print(`as_bm_pixmap.magick-image`(rl))
                  }, crayon = TRUE, unicode = TRUE)
})

test_that("`bm_extend.raster()`", {
    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    verify_output("txt/bm_extend_raster.txt", {
                  m <- matrix(c("blue", "yellow"), nrow = 2L, ncol = 2L)
                  r <- as.raster(m)
                  rt <- bm_extend(r, value = "red", top = 1L)
                  print(inherits(rt, "raster"))
                  bm_print(rt)
                  rr <- bm_extend(r, value = "red", right = 1L)
                  bm_print(rr)
                  rb <- bm_extend(r, value = "red", bottom = 1L)
                  bm_print(rb)
                  rl <- bm_extend(r, value = "red", left = 1L)
                  bm_print(rl)
                  }, crayon = TRUE, unicode = TRUE)
})

test_that("`bm_extend.nativeRaster()`", {
    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("farver")
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    verify_output("txt/bm_extend_nativeRaster.txt", {
                  m <- matrix(c("blue", "yellow"), nrow = 2L, ncol = 2L)
                  r <- as.raster(as_bm_pixmap(m), native = TRUE)
                  int <- farver::encode_native("red")
                  rt <- bm_extend(r, value = int, top = 1L)
                  bm_print(rt)
                  print(inherits(rt, "nativeRaster"))
                  rr <- bm_extend(r, value = int, right = 1L)
                  bm_print(rr)
                  rb <- bm_extend(r, value = int, bottom = 1L)
                  bm_print(rb)
                  rl <- bm_extend(r, value = int, left = 1L)
                  bm_print(rl)
                  }, crayon = TRUE, unicode = TRUE)
})
