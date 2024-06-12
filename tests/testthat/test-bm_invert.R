test_that("`bm_invert()`", {
    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    corn <- farming_crops_16x16()$corn$portrait
    corn_l <- bm_list(as_bm_bitmap(corn))
    corn_r <- as.raster(corn)
    corn_mi <- magick::image_read(corn_r)
    corn_nr <- as.raster(corn, native = TRUE)

    verify_output("txt/bm_invert.txt", {
        print(corn)
        print(bm_invert(corn))
        print(bm_invert(corn_l))
        bm_print(bm_invert(corn_r))
        bm_print(bm_invert(corn_mi))
        bm_print(bm_invert(corn_nr))
    }, crayon = TRUE, unicode = TRUE)
})
