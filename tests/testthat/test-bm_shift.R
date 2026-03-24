test_that("`bm_shift()`", {
	skip_if_not_installed("farver")
	skip_if_not_installed("magick")
	skip_if_not_installed("withr")
	withr::local_options(cli.unicode = TRUE)
	withr::local_options(bm_options(default = TRUE))

	crops <- farming_crops_16x16()
	corn <- crops$corn$portrait
	bm <- as_bm_bitmap(corn)
	bml <- bm_list(corn = bm)
	corn <- as_bm_pixmap(bm, col = c("transparent", "yellow"))
	corn_r <- as.raster(corn)
	corn_nr <- as.raster(corn, native = TRUE)
	corn_mi <- magick::image_read(corn)

	verify_output(
		"txt/bm_shift.txt",
		{
			print(bm_shift(bm, right = 1L), bg = "cyan")
			print(bm_shift(bml, right = 1L))
			bm_print(bm_shift(corn, left = 1L), bg = "cyan")
			bm_print(bm_shift(corn_r, top = 2L), bg = "cyan")
			bm_print(bm_shift(corn_nr, right = 2L), bg = "cyan")
			bm_print(bm_shift(corn_mi, bottom = 2L), bg = "cyan")
		},
		unicode = TRUE,
		crayon = TRUE
	)
})

test_that("`bm_shift()` overflow", {
	skip_if_not_installed("withr")
	withr::local_options(bm_options(default = TRUE))

	crops <- farming_crops_16x16()
	bm <- as_bm_bitmap(crops$corn$portrait)

	expect_snapshot(
		{
			bm_shift(bm, top = 6L, overflow = "error")
			bm_shift(bm, right = 6L, overflow = "error")
			bm_shift(bm, bottom = 6L, overflow = "error")
			bm_shift(bm, left = 6L, overflow = "error")
		},
		error = TRUE
	)

	clipped <- bm_shift(bm, right = 3L, overflow = "clip")
	expect_equal(dim(clipped), dim(bm))

	verify_output(
		"txt/bm_shift_overflow.txt",
		{
			print(bm_shift(bm, top = 4L, overflow = "wrap"))
			print(bm_shift(bm, right = 4L, overflow = "wrap"))
			print(bm_shift(bm, bottom = 4L, overflow = "wrap"))
			print(bm_shift(bm, left = 4L, overflow = "wrap"))
		},
		unicode = TRUE
	)
})
