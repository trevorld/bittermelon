font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
capital_r <- font[[str2ucp("R")]]

test_that("bm_trim()", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    capital_r_trimmed <- bm_trim(capital_r, c(1, 1, 3, 0))
    expect_equal(nrow(capital_r_trimmed), 12L)
    expect_equal(ncol(capital_r_trimmed), 7L)
    verify_output("txt/capital_r_trimmed.txt", {
        print(capital_r, px = c("-", "#"))
        print(bm_trim(capital_r, c(1, 1, 3, 0)),
              px = c("-", "#"))
        print(bm_trim(capital_r, height = 8, vjust = "top"),
              px = c("-", "#"))
        print(bm_trim(bm_list(capital_r), height = 8, vjust = "top"),
              px = c("-", "#"))
        print(bm_trim(capital_r, height = 8, vjust = "bottom"),
              px = c("-", "#"))
        print(bm_trim(capital_r, width = 4, hjust = "left"),
              px = c("-", "#"))
        print(bm_trim(capital_r, width = 4, hjust = "right"),
              px = c("-", "#"))
    })

    capital_r_resized <- bm_resize(capital_r, width = 10, height = 14, vjust = "top")
    expect_equal(ncol(capital_r_resized), 10L)
    expect_equal(nrow(capital_r_resized), 14L)

    capital_r_resized2 <- bm_resize(capital_r, width = 7, height = 18, hjust = "left")
    expect_equal(ncol(capital_r_resized2), 7L)
    expect_equal(nrow(capital_r_resized2), 18L)
    verify_output("txt/capital_r_resized.txt",
                  print(capital_r_resized, px = c("-", "#")))
    verify_output("txt/capital_r_resized2.txt",
                  print(capital_r_resized2, px = c("-", "#")))
})

test_that("bm_trim.bm_pixmap()", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    skip_if_not(cli::is_utf8_output())
    crops <- farming_crops_16x16()
    corn <- crops$corn$portrait
    verify_output("txt/bm_trim_pixmap.txt", {
        print(corn, compress = "v")
        print(bm_trim(corn, top = 8L), compress = "v")
        print(bm_trim(corn, right = 8L), compress = "v")
        print(bm_trim(corn, bottom = 8L), compress = "v")
        print(bm_trim(corn, left = 8L), compress = "v")
    }, crayon = FALSE, unicode = TRUE)
})

test_that("bm_trim.magick-image()", {
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    skip_if_not(cli::is_utf8_output())
    crops <- farming_crops_16x16()
    corn <- magick::image_read(crops$corn$portrait)
    verify_output("txt/bm_trim_magick.txt", {
        print(`as_bm_pixmap.magick-image`(corn), compress = "v")
        corn_t <- bm_trim(corn, top = 8L)
        print(inherits(corn_t, "magick-image"))
        print(`as_bm_pixmap.magick-image`(corn_t), compress = "v")
        corn_r <- bm_trim(corn, right = 8L)
        print(`as_bm_pixmap.magick-image`(corn_r), compress = "v")
        corn_b <- bm_trim(corn, bottom = 8L)
        print(`as_bm_pixmap.magick-image`(corn_b), compress = "v")
        corn_l <- bm_trim(corn, left = 8L)
        print(`as_bm_pixmap.magick-image`(corn_l), compress = "v")
    }, crayon = FALSE, unicode = TRUE)
})

test_that("bm_trim.nativeRaster()", {
    skip_if_not_installed("farver")
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    skip_if_not(cli::is_utf8_output())
    crops <- farming_crops_16x16()
    corn <- as.raster(crops$corn$portrait, native = TRUE)
    verify_output("txt/bm_trim_nativeRaster.txt", {
        bm_print(corn, compress = "v")
        corn_t <- bm_trim(corn, top = 8L)
        print(inherits(corn_t, "nativeRaster"))
        bm_print(corn_t, compress = "v")
        corn_r <- bm_trim(corn, right = 8L)
        bm_print(corn_r, compress = "v")
        corn_b <- bm_trim(corn, bottom = 8L)
        bm_print(corn_b, compress = "v")
        corn_l <- bm_trim(corn, left = 8L)
        bm_print(corn_l, compress = "v")
    }, crayon = FALSE, unicode = TRUE)
})

test_that("bm_trim.raster()", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    skip_if_not(cli::is_utf8_output())
    crops <- farming_crops_16x16()
    corn <- as.raster(crops$corn$portrait)
    verify_output("txt/bm_trim_raster.txt", {
        bm_print(corn, compress = "v")
        corn_t <- bm_trim(corn, top = 8L)
        print(inherits(corn_t, "raster"))
        bm_print(corn_t, compress = "v")
        corn_r <- bm_trim(corn, right = 8L)
        bm_print(corn_r, compress = "v")
        corn_b <- bm_trim(corn, bottom = 8L)
        bm_print(corn_b, compress = "v")
        corn_l <- bm_trim(corn, left = 8L)
        bm_print(corn_l, compress = "v")
    }, crayon = FALSE, unicode = TRUE)
})

test_that("bm_shift()", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    capital_r_shifted <- bm_shift(capital_r, bottom = 2L)
    verify_output("txt/capital_r_shifted.txt",
                  print(capital_r_shifted, px = c("-", "#")))
    expect_equal(nrow(capital_r_shifted), 16L)
    expect_equal(ncol(capital_r_shifted), 8L)
})

test_that("bm_shadow", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    verify_output("txt/capital_r_shadow.txt",
                  print(bm_shadow(capital_r), px = px_ascii))
    verify_output("txt/capital_r_bold.txt",
                  print(bm_bold(capital_r), px = px_ascii))
    verify_output("txt/capital_r_glow.txt",
                  print(bm_glow(capital_r), px = px_ascii))
    verify_output("txt/capital_r_glow_corner.txt",
                  print(bm_glow(capital_r, corner = TRUE), px = px_ascii))
})


test_that("bm_rotate()", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    verify_output("txt/capital_r_rotated90.txt",
                  print(bm_rotate(capital_r, 90), px = px_ascii))
    verify_output("txt/capital_r_rotated180.txt",
                  print(bm_rotate(capital_r, 180), px = px_ascii))
    verify_output("txt/capital_r_rotated270.txt",
                  print(bm_rotate(capital_r, 270), px = px_ascii))
    verify_output("txt/capital_r_rotatedm90.txt",
                  print(bm_rotate(capital_r, 90, clockwise = FALSE), px = px_ascii))
})

test_that("c()", {
    stats <- as_bm_list("STATS", font = font)

    expect_equal(c(capital_r), bm_list(capital_r))
    expect_equal(stats, c(stats))
    expect_equal(font, c(font))

    bbb <- c(capital_r, capital_r, capital_r)
    expect_false(is_bm_font(bbb))
    expect_true(is_bm_list(bbb))
    expect_length(bbb, 3L)

    bl <- c(capital_r, stats)
    expect_false(is_bm_font(bl))
    expect_true(is_bm_list(bl))
    expect_length(bl, 6L)

    bf <- c(`U+E000` = capital_r, font)
    expect_true(is_bm_font(bf))
    expect_length(bf, 838L)

    fbb <- c(font, `U+E000` = capital_r, `U+E001` = capital_r)
    expect_true(is_bm_font(fbb))
    expect_length(fbb, 839L)

    ff <- c(font, font)
    expect_true(is_bm_font(ff))
    expect_length(ff, 837L)

    fl <- c(font, stats)
    expect_true(is_bm_font(fl))
    expect_length(fl, 837L)

    lbb <- c(stats, capital_r, capital_r)
    expect_false(is_bm_font(lbb))
    expect_true(is_bm_list(lbb))
    expect_length(lbb, 7L)

    ll <- c(stats, stats)
    expect_false(is_bm_font(ll))
    expect_true(is_bm_list(ll))
    expect_length(ll, 10L)

    lf <- c(stats, font)
    expect_true(is_bm_font(lf))
    expect_length(lf, 837L)
})
