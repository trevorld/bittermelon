test_that("plot.bm_bitmap()", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign <- bm_bitmap(plus_sign)
    expect_doppelganger("plot_plus_sign", function() plot(plus_sign))
})
