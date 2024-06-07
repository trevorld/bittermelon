font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
capital_r <- font[[str2ucp("R")]]

test_that("`bm_padding_lengths()`", {
    # `bm_padding_lengths.bm_bitmap()` tested in `test-bm_list.R`

    corn <- farming_crops_16x16()$corn$portrait
    expected <- c(top = 2L, right = 2L, bottom = 2L, left = 1L)
    expect_equal(bm_padding_lengths(corn), expected)
    corn_l <- bm_list(as_bm_bitmap(corn))
    expect_equal(bm_padding_lengths(corn_l)[[1L]], expected)
    corn_lrs <- bm_resize(corn_l, width = 20L)
    expect_equal(bm_widths(corn_lrs), 20L)
    corn_e <- bm_pad(corn, type = "exact", sides = 2L)
    expect_equal(bm_widths(corn_e), 17L)
    corn_t <- bm_pad(corn, type = "exact", sides = 0L)
    expect_equal(bm_widths(corn_t), 13L)
    corn_rs <- bm_resize(corn_t, width = 16L)
    expect_equal(bm_widths(corn_rs), 16L)

    corn_r <- as.raster(corn)
    expect_equal(bm_padding_lengths(corn_r), expected)
    corn_re <- bm_pad(corn_r, type = "exact", sides = 2L)
    expect_equal(bm_widths(corn_re), 17L)
    corn_rt <- bm_pad(corn_r, type = "exact", sides = 0L)
    expect_equal(bm_widths(corn_rt), 13L)
    corn_rrs <- bm_resize(corn_rt, width = 16L)
    expect_equal(bm_widths(corn_rrs), 16L)

    skip_if_not_installed("farver")
    corn_nr <- as.raster(corn, native = TRUE)
    expect_equal(bm_padding_lengths(corn_nr), expected)
    corn_nre <- bm_pad(corn_nr, type = "exact", sides = 2L)
    expect_equal(bm_widths(corn_nre), 17L)
    corn_nrt <- bm_pad(corn_nr, type = "exact", sides = 0L)
    expect_equal(bm_widths(corn_nrt), 13L)
    corn_nrrs <- bm_resize(corn_nrt, width = 16L)
    expect_equal(bm_widths(corn_nrrs), 16L)

    skip_if_not_installed("magick")
    corn_mi <- magick::image_read(corn)
    expect_equal(bm_padding_lengths(corn_mi), expected)
    corn_mie <- bm_pad(corn_mi, type = "exact", sides = 2L)
    expect_equal(bm_widths(corn_mie), 17L)
    corn_mit <- bm_pad(corn_mi, type = "exact", sides = 0L)
    expect_equal(bm_widths(corn_mit), 13L)
    corn_mirs <- bm_resize(corn_mit, width = 16L)
    expect_equal(bm_widths(corn_mirs), 16L)
})

test_that("`bm_pad()`", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    capital_r_padded <- bm_pad(capital_r, sides = 2L)
    verify_output("txt/capital_r_padded.txt",
                  print(capital_r_padded, px = c(".", "#")))
    expect_equal(nrow(capital_r_padded), 14L)
    expect_equal(ncol(capital_r_padded), 11L)

    bml_padded <- bm_pad(bm_list(capital_r), sides = 2L)
    expect_equal(bm_heights(bml_padded), 14L)
    expect_equal(bm_widths(bml_padded), 11L)
})
