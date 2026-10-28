test_that("`tiff::readTIFF()` import works", {
	skip_if_not_installed("tiff")
	filename <- tempfile(fileext = ".tiff")
	on.exit(unlink(filename))
	corn <- farming_crops_16x16()$corn$portrait
	bm <- as_bm_bitmap(corn)
	pm <- as_bm_pixmap(bm, col = c("white", "green4"))

	tiff::writeTIFF(as.array(pm)[,, 1:3], filename)
	array_rgb <- tiff::readTIFF(filename)
	expect_equal(as_bm_pixmap(array_rgb), pm)
	expect_equal(as_bm_bitmap(array_rgb), bm)

	tiff::writeTIFF(as.array(corn), filename)
	# libtiff warns about `ExtraSamples` for the alpha channel
	array_rgba <- suppressWarnings(tiff::readTIFF(filename))
	expect_equal(as_bm_pixmap(array_rgba), corn)
	expect_equal(as_bm_bitmap(array_rgba), bm)
})
