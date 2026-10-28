test_that("`png::readPNG()` import works", {
	skip_if_not_installed("farver")
	filename <- tempfile(fileext = ".png")
	on.exit(unlink(filename))
	corn <- farming_crops_16x16()$corn$portrait
	bm <- as_bm_bitmap(corn)
	png::writePNG(as.array(corn), filename)

	array_num <- png::readPNG(filename)
	expect_equal(as_bm_pixmap(array_num), corn)
	expect_equal(as_bm_bitmap(array_num), bm)

	nr <- png::readPNG(filename, native = TRUE)
	expect_s3_class(nr, "nativeRaster")
	expect_equal(as_bm_pixmap(nr), corn)
	expect_equal(as_bm_bitmap(nr), bm)
})
