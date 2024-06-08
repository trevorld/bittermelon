test_that("`fontr` import works", {
    skip_if_not_installed("fontr", "0.1")

    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    ch_R <- fontr::glyph_bitmap("R", family = "sans",
                                face = "regular", pixel_size = 20)
    bm_R <- as_bm_bitmap(ch_R)
    pm_R <- as_bm_pixmap(ch_R)

    verify_output("txt/fontr.txt", {
        print(bm_R)
        print(pm_R)
    }, unicode = FALSE, crayon = FALSE)

})
