test_that("`pixeltrix` import works", {
    skip_if_not_installed("pixeltrix", "0.2.2")

    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    crops <- farming_crops_16x16()
    corn <- crops$corn$portrait
    bm <- as_bm_bitmap(corn)
    bmg <- bm_glow(bm)

    pt <- pixeltrix::as_pixeltrix(as.matrix(bmg, first_row_is_top = TRUE))
    expect_true(inherits(pt, "pixeltrix"))

    verify_output("txt/pixeltrix.txt", {
        bm <- as_bm_bitmap(pt)
        print(bm, compress = "n")
        pm <- as_bm_pixmap(pt)
        print(pm, compress = "n")
    }, unicode = TRUE, crayon = TRUE)
})
