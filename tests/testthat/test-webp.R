test_that("`webp::read_webp()` import works", {
    skip_if_not_installed("webp")

    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    filename <- tempfile(fileext = ".webp")
    on.exit(unlink(filename))

    corn <- farming_crops_16x16()$corn$portrait
    webp::write_webp(as.array(corn), filename, quality = NA)

    array_raw <- webp::read_webp(filename, numeric = FALSE)
    array_num <- webp::read_webp(filename, numeric = TRUE)
    pmr <- as_bm_pixmap(array_raw)
    expect_equal(corn, pmr)
    pmn <- as_bm_pixmap(array_num)
    expect_equal(corn, pmn)

    bm <- as_bm_bitmap(corn)
    bmr <- as_bm_bitmap(array_raw)
    expect_equal(bm, bmr)
    bmn <- as_bm_bitmap(array_num)
    expect_equal(bm, bmn)
})
