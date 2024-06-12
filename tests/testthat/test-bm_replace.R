test_that("`bm_replace()`", {
    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    m <- matrix(1L, ncol = 2L, nrow = 2L)
    bm <- as_bm_bitmap(m)
    bm <- bm_extend(bm, sides = 1L)

    pm <- as_bm_pixmap(bm)
    r <- as.raster(pm)
    nr <- as.raster(pm, native = TRUE)
    mi <- magick::image_read(pm)

    verify_output("txt/bm_replace.txt", {
        print(bm_replace(bm_list(bm), 2L))
        print(bm_replace(pm, "red"))
        rr <- bm_replace(r, "blue")
        bm_print(rr)
        nrr <- bm_replace(nr, "green")
        bm_print(nrr)
        mir <- bm_replace(mi, "orange")
        bm_print(mir)
    }, unicode = TRUE, crayon = TRUE)
})
