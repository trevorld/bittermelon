test_that("`bm_expand()` works", {
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

    pm0 <- bm_expand(corn, width = 0L, height = 1L)
    expect_equal(dim(pm0), c(16L, 0L))
    r0 <- bm_expand(corn_r, height = 0L)
    expect_equal(dim(r0), c(0L, 16L))

    expect_equal(bm_widths(bm_expand(corn_l, width = 3L)), 48L)

    verify_output("txt/bm_expand.txt", {
        print(corn, compress = "v")
        print(bm_expand(corn, width = 2L, height = 1L), compress = "v")
        print(bm_expand(corn, width = 1L, height = 2L), compress = "v")
        print(as_bm_pixmap(bm_expand(corn_r, width = 2L, height = 1L)), compress = "v")
        print(as_bm_pixmap(bm_expand(corn_r, height = 2L)), compress = "v")
        print(as_bm_pixmap(bm_expand(corn_nr, width = 2L, height = 1L)), compress = "v")
        print(as_bm_pixmap(bm_expand(corn_nr, height = 2L)), compress = "v")
        print(as_bm_pixmap(bm_expand(corn_mi, width = 2L, height = 1L)), compress = "v")
        print(as_bm_pixmap(bm_expand(corn_mi, height = 2L)), compress = "v")
    }, unicode = TRUE, crayon = FALSE)
})
