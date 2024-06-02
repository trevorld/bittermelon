test_that("`bm_widths()` and `bm_heights()`", {
    pm <- farming_crops_16x16()$corn$portrait
    bm <- as_bm_bitmap(pm)
    r <- as.raster(pm)

    expect_equal(bm_widths(pm), 16L)
    expect_equal(bm_widths(bm), 16L)
    expect_equal(bm_widths(r), 16L)

    expect_equal(bm_heights(pm), 16L)
    expect_equal(bm_heights(bm), 16L)
    expect_equal(bm_heights(r), 16L)

    skip_if_not_installed("farver")
    nr_bm <- as.raster(bm, native = TRUE)
    expect_equal(bm_widths(nr_bm), 16L)
    expect_equal(bm_heights(nr_bm), 16L)

    nr_pm <- as.raster(pm, native = TRUE)
    expect_equal(bm_widths(nr_pm), 16L)
    expect_equal(bm_heights(nr_pm), 16L)

    skip_if_not_installed("magick")
    mi <- magick::image_read(pm)
    expect_equal(bm_widths(mi), 16L)
    expect_equal(bm_heights(mi), 16L)
})
