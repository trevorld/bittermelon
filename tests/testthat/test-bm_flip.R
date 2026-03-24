test_that("`bm_flip()`", {
	skip_if_not_installed("farver")
	skip_if_not_installed("magick")
	skip_if_not_installed("withr")
	withr::local_options(cli.unicode = TRUE)
	withr::local_options(bm_options(default = TRUE))

	corn <- farming_crops_16x16()$corn$portrait
	corn_l <- bm_list(as_bm_bitmap(corn))
	expect_equal(
		bm_padding_lengths(corn_l[[1L]])[["left"]],
		bm_padding_lengths(bm_flip(corn_l, "h")[[1L]])[["right"]]
	)
	corn_r <- as.raster(corn)
	corn_mi <- magick::image_read(corn_r)
	corn_nr <- as.raster(corn, native = TRUE)

	verify_output(
		"txt/bm_flip.txt",
		{
			print(corn)
			print(bm_flip(corn, "h"))
			print(bm_flip(corn, "v"))
			print(bm_flip(corn, "b"))
			bm_print(bm_flip(corn_r, "h"))
			bm_print(bm_flip(corn_mi, "b"))
			bm_print(bm_flip(corn_nr, "h"))
		},
		crayon = FALSE,
		unicode = TRUE
	)
})
