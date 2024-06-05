test_that("`bm_pixmap()` works as expected", {
    pm <- as_bm_pixmap(matrix(character(0L), 0L, 0L))
    expect_true(is_bm_pixmap(pm))
    expect_true(is_bm_matrix(pm))

})

test_that("`as.array.bm_pixmap()`", {
    skip_if_not_installed("png")
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))

    tulip <- farming_crops_16x16()$tulip$portrait
    bm <- as_bm_bitmap(tulip)
    pm1 <- as_bm_pixmap(bm)
    a1 <- as.array(bm)
    f <- tempfile(fileext = ".png")
    png::writePNG(a1, f)
    a2 <- png::readPNG(f)
    unlink(f)
    pm2 <- as_bm_pixmap(a2)
    expect_equal(pm1, pm2)
})

test_that("`as_bm_pixmap()`", {
    skip_if_not_installed("farver")
    r0 <- as.raster(matrix(character(0L), nrow = 0L, ncol = 4L))
    pm0 <- as_bm_pixmap(r0)
    bm0 <- as_bm_bitmap(pm0)
    pm1 <- as_bm_pixmap(as.matrix(bm0))
    nr0 <- as.raster(pm0, native = TRUE)
    pm2 <- as_bm_pixmap(nr0)
    expect_equal(dim(pm0), c(0L, 4L))
    expect_equal(dim(pm1), c(0L, 4L))
    expect_equal(dim(pm2), c(0L, 4L))
})

test_that("`cbind()` and `rbind()`", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    skip_if_not(cli::is_utf8_output())
    fc <- farming_crops_16x16()
    avocado <- fc$avocado$portrait
    tulip <- fc$tulip$portrait
    verify_output("txt/bind_bm_pixmap.txt", {
       print(cbind(avocado, tulip), compress = "v")

       print( rbind(avocado, tulip), compress = "v")
    }, crayon = FALSE, unicode = TRUE)
})
