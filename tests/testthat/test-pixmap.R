test_that("{pixmap} import works", {
    skip_if_not_installed("pixmap")
    suppressWarnings({ # 
    # Warning in rep(cellres, length = 2) :
    # 'x' is NULL so the result will be NULL
    xi <- pixmap::pixmapIndexed(rep(1:8, 9), 
                                nrow=6, 
                                col=grDevices::terrain.colors(8))
    frgb <- system.file("pictures/logo.ppm", package="pixmap")[1]
    xrgb <- pixmap::read.pnm(frgb)
    fgm <- system.file("pictures/logo.pgm", package="pixmap")[1]
    xgm <- pixmap::read.pnm(fgm)
    fbm <- system.file("pictures/logo.pbm", package="pixmap")[1]
    xbm <- pixmap::read.pnm(fbm)
    })

    pm_i <- as_bm_pixmap(xi)
    pm_rgb <- as_bm_pixmap(xrgb)
    pm_gm <- as_bm_pixmap(xgm)
    pm_bm <- as_bm_pixmap(xbm)

    bm_i <- as_bm_bitmap(xi)
    bm_rgb <- as_bm_bitmap(xrgb)
    bm_gm <- as_bm_bitmap(xgm)
    bm_bm <- as_bm_bitmap(xbm)
    bm_bm2 <- as_bm_bitmap(xbm, mode = "luminance")

    #### Better tests?
    expect_true(is_bm_bitmap(bm_i))
    expect_true(is_bm_bitmap(bm_rgb))
    expect_true(is_bm_bitmap(bm_gm))
    expect_true(is_bm_bitmap(bm_bm))

    expect_true(is_bm_pixmap(pm_i))
    expect_true(is_bm_pixmap(pm_rgb))
    expect_true(is_bm_pixmap(pm_gm))
    expect_true(is_bm_pixmap(pm_bm))

    pm_bm2 <- as_bm_pixmap(bm_bm, col = c("white", "black"))
    expect_equal(pm_bm, pm_bm2)

    skip_on_cran()
    skip_if_not_installed("magick")
    mi_rgb <- magick::image_read(frgb)
    mi_gm <- magick::image_read(fgm)
    mi_bm <- magick::image_read(fbm)
    expect_equal(pm_rgb, as_bm_pixmap(mi_rgb))
    expect_equal(pm_gm, as_bm_pixmap(mi_gm))
    expect_equal(pm_bm, as_bm_pixmap(mi_bm))
})
