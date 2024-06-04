test_that("`bm_overlay()`", {
    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    crops <- farming_crops_16x16()
    corn <- crops$corn$portrait
    grapes <- crops$grapes$portrait

    space <- as_bm_bitmap(matrix(0L, nrow = 16L, ncol = 16L))
    bm1 <- as_bm_bitmap(corn)
    bm2 <- as_bm_bitmap(grapes)

    bml <- bm_list(corn = bm1, grapes = bm2)

    corn <- as_bm_pixmap(bm1, col = c("transparent", "yellow"))
    grapes <- as_bm_pixmap(bm2, col = c("transparent", "magenta"))

    corn_r <- as.raster(corn)
    grapes_r <- as.raster(grapes)

    corn_nr <- as.raster(corn, native = TRUE)
    grapes_nr <- as.raster(grapes, native = TRUE)

    corn_mi <- magick::image_read(corn)
    grapes_mi <- magick::image_read(grapes)

    verify_output("txt/bm_overlay.txt", {
        print(bm_overlay(bml, space))
        print(bm_overlay(bml, under = space))

        print(bm_overlay(corn, grapes))
        print(bm_overlay(corn, under = grapes))

        print(as_bm_pixmap(bm_overlay(corn_r, grapes_r)))
        print(as_bm_pixmap(bm_overlay(corn_r, under = grapes_r)))

        print(as_bm_pixmap(bm_overlay(corn_nr, grapes_nr)))
        print(as_bm_pixmap(bm_overlay(corn_nr, under = grapes_nr)))

        print(as_bm_pixmap(bm_overlay(corn_mi, grapes_mi)))
        print(as_bm_pixmap(bm_overlay(corn_mi, under = grapes_mi)))

        print(bm_overlay(bm_extend(corn, width = 18L),
                         grapes))
        print(bm_overlay(corn,
                         bm_extend(grapes, width = 18L)))
        print(bm_overlay(bm_extend(corn, height = 18L),
                         grapes))
        print(bm_overlay(corn,
                         bm_extend(grapes, height = 18L)))
    }, unicode = TRUE, crayon = TRUE)
})

test_that("`bm_shadow()`, `bm_bold()`, and `bm_glow()`", {
    skip_if_not_installed("farver")
    skip_if_not_installed("magick")
    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    crops <- farming_crops_16x16()
    corn <- crops$corn$portrait
    bm <- as_bm_bitmap(corn)
    corn <- as_bm_pixmap(bm, col = c("transparent", "yellow"))
    bml <- bm_list(corn = bm)
    corn_r <- as.raster(corn)
    corn_nr <- as.raster(corn, native = TRUE)
    corn_mi <- magick::image_read(corn)

    verify_output("txt/bm_shadow.txt", {
        print(bm_shadow(bml))
        print(bm_shadow(corn, "red"))
        rs <- bm_shadow(corn_r, "magenta")
        print(as_bm_pixmap(rs))
        nrs <- bm_shadow(corn_nr, "green")
        print(as_bm_pixmap(nrs))
        mis <- bm_shadow(corn_mi, "blue")
        print(as_bm_pixmap(mis))

        print(bm_bold(bml))
        print(bm_bold(corn, "red"))
        rb <- bm_bold(corn_r, "magenta")
        print(as_bm_pixmap(rb))
        nrb <- bm_bold(corn_nr, "green")
        print(as_bm_pixmap(nrb))
        mib <- bm_bold(corn_mi, "blue")
        print(as_bm_pixmap(mib))

        print(bm_glow(bml))
        print(bm_glow(bml, extend = FALSE))
        print(bm_glow(corn, "red"))
        rg <- bm_glow(corn_r, "magenta")
        print(as_bm_pixmap(rg))
        nrg <- bm_glow(corn_nr, "green")
        print(as_bm_pixmap(nrg))
        mig <- bm_glow(corn_mi, "blue", corner = TRUE)
        print(as_bm_pixmap(mig))
    }, unicode = TRUE, crayon = TRUE)
})
