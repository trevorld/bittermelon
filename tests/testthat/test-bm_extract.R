test_that("bm_extract()", {
	skip_if_not_installed("withr")
	withr::local_options(cli.unicode = TRUE)
	withr::local_options(bm_options(default = TRUE))

	crops <- farming_crops_16x16()
	corn <- crops$corn$portrait

	corn_top <- bm_extract(corn, 9:16)
	corn_bot <- bm_extract(corn, 1:8)
	corn_left <- bm_extract(corn, cols = 1:8)
	corn_right <- bm_extract(corn, cols = 9:16)

	verify_output(
		"txt/bm_extract_pixmap.txt",
		{
			print(corn, compress = "v")
			print(corn_top, compress = "v")
			print(corn_bot, compress = "v")
			print(corn_left, compress = "v")
			print(corn_right, compress = "v")
		},
		crayon = FALSE,
		unicode = TRUE
	)

	corn_l_top <- bm_extract(bm_list(corn), 9:16)
	expect_equal(corn_top, corn_l_top[[1L]])

	corn_r_top <- bm_extract(as.raster(corn), 9:16)
	expect_equal(corn_top, as_bm_pixmap(corn_r_top))

	skip_if_not_installed("magick")
	corn_m_top <- bm_extract(magick::image_read(corn), 9:16)
	expect_equal(corn_top, as_bm_pixmap(corn_m_top))

	skip_if_not_installed("farver")
	corn_nr_top <- bm_extract(as.raster(corn, native = TRUE), 9:16)
	expect_equal(corn_top, as_bm_pixmap(corn_nr_top))
})
