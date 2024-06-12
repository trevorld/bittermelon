test_that("bm_bitmap()", {

    expect_error(bm_bitmap("Zippity"))
    expect_error(bm_bitmap(matrix("Zippity")))

    space_matrix <- matrix(0L, nrow = 16, ncol = 16)
    space_glyph <- bm_bitmap(space_matrix)
    expect_true(is_bm_bitmap(space_glyph))
    expect_false(is_bm_bitmap(space_matrix))
    expect_equal(nrow(space_glyph), 16)
    expect_equal(ncol(space_glyph), 16)

    space_glyph2 <- bm_bitmap(space_glyph)
    expect_equal(space_glyph, space_glyph2)

    space_glyph3 <- as_bm_bitmap(space_matrix)
    expect_equal(space_glyph, space_glyph3)

    space_matrix2 <- as.matrix(space_glyph)
    expect_equal(space_matrix, space_matrix2)
})

test_that("as_bm_bitmap()", {
    skip_if_not(capabilities("png"))
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    circle <- as_bm_bitmap(grid::circleGrob(r = 0.25), width = 16L, height = 16L)
    verify_output("txt/circle_grob.txt", print(circle, px = c(".", "@")))

    circle_outline <- bm_outline(circle)
    verify_output("txt/circle_bm_outline.txt",
                  print(circle_outline, px = px_ascii))

    skip_if_not_installed("mazing")
    set.seed(42)
    maze <- mazing::maze(12L, 12)
    verify_output("txt/maze.txt", {
        print(as_bm_bitmap(maze), px = px_ascii)
        print(as_bm_bitmap(maze, walls = TRUE), px = px_ascii)
    }, unicode = FALSE)

    maze2 <- as_bm_bitmap(mazing::maze(12L, 12L), start = "top", end = "bottom")
    verify_output("txt/maze_solution.txt", print(maze2, px = px_ascii))

    skip_if_not_installed("farver")
    bm0 <- bm_bitmap(matrix(integer(0L), nrow = 0L, ncol = 4L))
    bm1 <- as_bm_bitmap(as.raster(bm0))
    bm2 <- as_bm_bitmap(as.raster(bm0, native = TRUE))
    expect_equal(dim(bm0), c(0L, 4L))
    expect_equal(dim(bm1), c(0L, 4L))
    expect_equal(dim(bm2), c(0L, 4L))

    skip_if_not_installed("magick")
    tulip <- farming_crops_16x16()$tulip$portrait
    mi_rgba <- magick::image_read(tulip)
    bm0 <- as_bm_bitmap(tulip)
    bm1 <- as_bm_bitmap(mi_rgba)
    expect_equal(bm0, bm1)

    skip_on_cran()
    f <- tempfile(fileext = ".png")
    mi_g <- magick::image_read(as.raster(bm0, col = c("white", "black")))
    magick::image_write(mi_g, f)
    bm2 <- as_bm_bitmap(as_bm_pixmap(png::readPNG(f)), mode = "darkness")
    expect_equal(bm0, bm2)

    mi_ga <- magick::image_read(as.raster(bm0, col = c("transparent", "black")))
    magick::image_write(mi_ga, f)
    bm3 <- as_bm_bitmap(png::readPNG(f))
    expect_equal(bm0, bm3)

    mi_rgb <- magick::image_read(as.raster(bm0, col = c("white", "green4")))
    magick::image_write(mi_rgb, f)
    bm4 <- as_bm_bitmap(as_bm_pixmap(png::readPNG(f)), mode = "darkness")
    bm5 <- as_bm_bitmap(png::readPNG(f))
    bm6 <- as_bm_bitmap(as_bm_pixmap(png::readPNG(f)), mode = "brightness")
    expect_equal(bm0, bm4)
    expect_equal(bm_invert(bm4), bm6)
    unlink(f)
})
