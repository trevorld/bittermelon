test_that("`bm_shift()`", {
    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    crops <- farming_crops_16x16()
    corn <- crops$corn$portrait
    bm <- as_bm_bitmap(corn)
    bml <- bm_list(corn = bm)
    corn <- as_bm_pixmap(bm, col = c("transparent", "yellow"))
    corn_r <- as.raster(corn)
    corn_nr <- as.raster(corn, native = TRUE)
    corn_mi <- magick::image_read(corn)

    verify_output("txt/bm_shift.txt", {
        print(bm_shift(bm, right = 1L), bg = "cyan")
        print(bm_shift(bml, right = 1L))
        bm_print(bm_shift(corn, left = 1L), bg = "cyan")
        bm_print(bm_shift(corn_r, top = 2L), bg = "cyan")
        bm_print(bm_shift(corn_nr, right = 2L), bg = "cyan")
        bm_print(bm_shift(corn_mi, bottom = 2L), bg = "cyan")
    }, unicode = TRUE, crayon = TRUE)
})
