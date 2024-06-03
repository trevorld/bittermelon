test_that("`bm_flip()`", {
    skip_if_not_installed("magick")
    skip_if_not_installed("farver")

    corn <- farming_crops_16x16()$corn$portrait
    corn_l <- bm_list(as_bm_bitmap(corn))
    expect_equal(bm_padding_lengths(corn_l[[1L]])[["left"]],
                 bm_padding_lengths(bm_flip(corn_l, "h")[[1L]])[["right"]])
    corn_r <- as.raster(corn)
    corn_mi <- magick::image_read(corn_r)
    corn_nr <- as.raster(corn, native = TRUE)

    expect_equal(bm_padding_lengths(corn_l[[1L]])[["left"]],
                 bm_padding_lengths(bm_rotate(corn_l, 90L)[[1L]])[["top"]])

    verify_output("txt/bm_flip.txt", {
        print(corn)
        print(bm_flip(corn, "h"))
        print(bm_flip(corn, "v"))
        print(bm_flip(corn, "b"))
        print(as_bm_pixmap(bm_flip(corn_r, "h")))
        print(as_bm_pixmap(bm_flip(corn_mi, "h")))
        print(as_bm_pixmap(bm_flip(corn_nr, "h")))
    }, crayon = FALSE, unicode = TRUE)

    verify_output("txt/bm_rotate.txt", {
        print(bm_rotate(corn, 0L))
        print(bm_rotate(corn, 90L))
        print(bm_rotate(corn, 180L))
        print(bm_rotate(corn, 270L))
        print(as_bm_pixmap(bm_rotate(corn_r, 0L)))
        print(as_bm_pixmap(bm_rotate(corn_r, 90L)))
        print(as_bm_pixmap(bm_rotate(corn_r, 180L)))
        print(as_bm_pixmap(bm_rotate(corn_r, 270L)))
        print(as_bm_pixmap(bm_rotate(corn_mi, 90L)))
        print(as_bm_pixmap(bm_rotate(corn_nr, 90L)))
    }, crayon = FALSE, unicode = TRUE)
})
