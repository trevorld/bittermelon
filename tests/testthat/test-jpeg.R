test_that("`jpeg::readJPEG()` import works", {
	skip_if_not_installed("jpeg")
	filename <- tempfile(fileext = ".jpg")
	on.exit(unlink(filename))
	bm <- as_bm_bitmap(farming_crops_16x16()$corn$portrait)
	pm <- as_bm_pixmap(bm, col = c("white", "green4"))
	jpeg::writeJPEG(as.array(pm)[,, 1:3], filename, quality = 1)

	array_num <- jpeg::readJPEG(filename)
	expect_equal(dim(array_num), c(16L, 16L, 3L))
	expect_equal(dim(as_bm_pixmap(array_num)), dim(pm))
	expect_equal(as_bm_bitmap(array_num), bm)
})
