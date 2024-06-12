font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)

test_that("bm_mask()", {
    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    one <- font[[str2ucp("1")]]
    circle_large <- as_bm_bitmap(grid::circleGrob(r = 0.50), width = 16L, height = 16L)
    circle_small <- as_bm_bitmap(grid::circleGrob(r = 0.40), width = 16L, height = 16L)

    circle_outline <- bm_mask(circle_large, circle_small)

    # U+2776 "Dingbat Negative Circled Digit One"
    circle_minus_one <- bm_mask(circle_large, one)

    square_full <- bm_bitmap(matrix(1L, nrow = 16L, ncol = 16L))
    square_minus_lower_left <- square_full
    square_minus_lower_left[1:8, 1:8] <- 0L
    circle_minus_lower_left <- bm_mask(circle_large, square_minus_lower_left, mode = "alpha")

    rainbow <- matrix(grDevices::rainbow(8L), byrow = TRUE, ncol = 8L, nrow = 8L)
    pm <- bm_expand(as_bm_pixmap(rainbow), 2L)
    r <- as.raster(pm)
    nr <- as.raster(pm, native = TRUE)
    mi <- magick::image_read(pm)

    pmm <- bm_mask(pm, circle_large, mode = "alpha")
    rm <- bm_mask(r, circle_small, mode = "luminance")
    nrm <- bm_mask(nr, circle_minus_lower_left, mode = "alpha")
    mim <- bm_mask(mi, square_minus_lower_left, mode = "luminance")

    verify_output("txt/bm_mask.txt", {
        print(circle_outline, px = px_ascii)
        print(circle_minus_one, px = px_ascii)
        print(circle_minus_lower_left, px = px_ascii)
        print(pmm, compress = "v")
        bm_print(rm, compress = "v")
        bm_print(nrm, compress = "v")
        bm_print(mim, compress = "v")
    }, unicode = TRUE, crayon = TRUE)
})
