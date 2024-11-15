test_that("`plot.bm_bitmap`()", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")

    plus_sign <- matrix(0L, nrow = 9L, ncol = 9L)
    plus_sign[5L, 3:7] <- 1L
    plus_sign[3:7, 5L] <- 1L
    plus_sign <- bm_bitmap(plus_sign)
    expect_doppelganger("plot_plus_sign", function() plot(plus_sign))
})

test_that("`plot.bm_pixmap`()", {
    skip_if_not_installed("vdiffr")
    library("vdiffr")
    grapes <- farming_crops_16x16()$grapes$portrait
    expect_doppelganger("grapes", function() plot(grapes))
})

test_that("`as.data.frame.bm_bitmap`()", {

font_file <- system.file("fonts/fixed/4x6.yaff.gz", package = "bittermelon")
font <- read_yaff(font_file)
bm <- as_bm_bitmap("RSTATS", font = font)
df <- as.data.frame(bm, filtrate = 1L)
expect_true(is.data.frame(df))
expect_doppelganger("as.data.frame.bm_bitmap", function()
  grid::grid.rect(df$x * 0.6, df$y * 0.6, width = 0.5, height = 0.5,
                  gp = grid::gpar(fill = 'black'), default.units = 'cm')
  )
})

test_that("`as.data.frame.bm_pixmap`()", {
  corn <- farming_crops_16x16()$corn$portrait
  df <- as.data.frame(corn)
  expect_true(is.data.frame(df))
  expect_doppelganger("as.data.frame.bm_pixmap", function()
    grid::grid.circle(df$x * 0.6, df$y * 0.6, r = 0.25,
                      gp = grid::gpar(fill = df$value), default.units = 'cm')
    )
})

test_that("`bm_pixel_picker_plot()`", {
  corn <- farming_crops_16x16()$corn$portrait
  expect_doppelganger("bm_pixel_picker_plot", function()
    bm_pixel_picker_plot(corn)
    )
})
