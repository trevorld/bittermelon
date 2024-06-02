test_that("`bm_flip()`", {
    corn <- farming_crops_16x16()$corn$portrait
    corn_r <- as.raster(corn)

    skip_if_not_installed("magick")
    corn_mi <- magick::image_read(corn_r)

    skip_if_not_installed("farver")
    corn_nr <- as.raster(corn, native = TRUE)

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
