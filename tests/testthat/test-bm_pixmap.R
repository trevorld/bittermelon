test_that("`bm_pixmap()` works as expected", {
    pm <- as_bm_pixmap(matrix("transparent", 0L, 0L))
    expect_true(is_bm_pixmap(pm))
    expect_true(is_bm_matrix(pm))
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
