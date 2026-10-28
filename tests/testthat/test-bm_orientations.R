font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
corn <- farming_crops_16x16()$corn$portrait
corn_wide <- bm_extend(corn, width = 20L)
corn_tall <- bm_extend(corn, height = 20L)
bml <- bm_list(corn, corn_wide, corn_tall)

test_that("bm_orientations()", {
	expect_equal(bm_orientations(font), "portrait")
	expect_length(bm_orientations(font, unique = FALSE), length(font))
	expect_equal(bm_orientations(bm_rotate(font, 90)), "landscape")
	expect_equal(bm_orientations(corn), "square")
	expect_equal(bm_orientations(corn, square = "portrait"), "portrait")
	expect_equal(bm_orientations(corn, square = "landscape"), "landscape")
	expect_equal(bm_orientations(corn_wide), "landscape")
	expect_equal(bm_orientations(corn_tall), "portrait")
	expect_snapshot(bm_orientations(corn, square = "circle"), error = TRUE)

	expect_equal(bm_orientations(bml), c("square", "landscape", "portrait"))
	expect_equal(
		bm_orientations(bml, unique = FALSE),
		c("square", "landscape", "portrait")
	)
	expect_equal(
		bm_orientations(bml, square = "portrait"),
		c("portrait", "landscape")
	)
	expect_equal(
		bm_orientations(bm_lapply(bml, as_bm_bitmap)),
		c("square", "landscape", "portrait")
	)
	expect_equal(bm_orientations(as.raster(corn_wide)), "landscape")
	expect_equal(bm_orientations(as.raster(corn)), "square")
})

test_that("bm_orientations() nativeRaster method", {
	skip_if_not_installed("farver")
	expect_equal(bm_orientations(as.raster(corn_tall, native = TRUE)), "portrait")
	expect_equal(bm_orientations(as.raster(corn, native = TRUE)), "square")
})

test_that("bm_orientations() magick-image method", {
	skip_if_not_installed("magick")
	expect_equal(bm_orientations(magick::image_read(corn)), "square")
	expect_equal(bm_orientations(magick::image_read(corn), square = "portrait"), "portrait")
	expect_equal(bm_orientations(magick::image_read(corn_wide)), "landscape")
	expect_equal(
		bm_orientations(bm_lapply(bml, magick::image_read)),
		c("square", "landscape", "portrait")
	)
})
