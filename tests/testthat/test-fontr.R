test_that("`fontr` import works", {
    skip_if_not_installed("fontr", "0.1")

    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    ch_C <- fontr::glyph_bitmap("C", family = "sans",
                                face = "regular", pixel_size = 20)
    bm_C <- as_bm_bitmap(ch_C)
    pm_C <- as_bm_pixmap(ch_C)

    verify_output("txt/fontr.txt", {
        print(bm_C)
        print(pm_C)
    }, unicode = FALSE, crayon = FALSE)

})
