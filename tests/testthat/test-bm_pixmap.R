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
    bm2 <- tulip != col2hex("transparent")
    expect_equal(bm, bm2)

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
    skip_if_not_installed("magick")
    skip_if_not(capabilities("png"))
    m0 <- matrix(character(0L), nrow = 0L, ncol = 4L)
    expect_equal(nrow(flip_matrix_vertically(m0)), 0L)
    expect_equal(ncol(flip_matrix_horizontally(t(m0))), 0L)
    r0 <- as.raster(m0)
    pm0 <- as_bm_pixmap(r0)
    pm1 <- as_bm_pixmap(as.matrix(pm0))
    nr0 <- as.raster(pm0, native = TRUE)
    pm2 <- as_bm_pixmap(nr0)
    pm3 <- as_bm_pixmap(as.matrix(as_bm_bitmap(pm0)))
    expect_equal(dim(pm0), c(0L, 4L))
    expect_equal(dim(pm1), c(0L, 4L))
    expect_equal(dim(pm2), c(0L, 4L))
    expect_equal(dim(pm3), c(0L, 4L))

    grob <- grid::circleGrob(r = 0.4, gp = grid::gpar(fill = "red", lwd = 2, col = "black"))
    pm <- as_bm_pixmap(grob)
    expect_equal(dim(pm), c(16L, 16L))
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

test_that("`c()`", {
    tulip <- farming_crops_16x16()$tulip$portrait
    bml1 <- c(tulip, tulip)
    bml2 <- c(tulip, tulip, tulip)
    bml3 <- c(tulip, bml2)
    bml4 <- c(bml2, bml3)
    bml5 <- c(tulip)

    expect_equal(length(bml1), 2L)
    expect_equal(length(bml2), 3L)
    expect_equal(length(bml3), 4L)
    expect_equal(length(bml4), 7L)
    expect_equal(length(bml5), 1L)
    expect_true(all(vapply(bml4, is_bm_pixmap, logical(1L))))
})
