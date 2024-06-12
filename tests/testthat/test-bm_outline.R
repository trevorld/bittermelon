test_that("`bm_outline()` works as expected", {

    bm0 <- bm_bitmap(matrix(integer(), nrow = 4L, ncol = 0L))
    expect_equal(bm0, bm_outline(bm0))

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

    verify_output("txt/bm_outline.txt", {
        bm_print(as_bm_bitmap(corn), compress = "v")
        print(bm_outline(corn_l), compress = "v")
        print(bm_outline(corn, "red"), compress = "v")
        corn_ro <- bm_outline(corn_r, "blue")
        bm_print(corn_ro, compress = "v")
        corn_nro <- bm_outline(corn_nr, col2int("green"))
        bm_print(corn_nro, compress = "v")
        corn_mio <- bm_outline(corn_mi, "orange")
        bm_print(corn_mio, compress = "v")
    }, unicode = TRUE, crayon = TRUE)
})
