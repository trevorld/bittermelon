font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
capital_r <- font[[str2ucp("R")]]

test_that("`bm_rotate()`", {
	skip_if_not_installed("farver")
	skip_if_not_installed("magick")
	skip_if_not_installed("withr")
	withr::local_options(cli.unicode = TRUE)
	withr::local_options(bm_options(default = TRUE))

	corn <- farming_crops_16x16()$corn$portrait
	corn_l <- bm_list(as_bm_bitmap(corn))
	corn_r <- as.raster(corn)
	corn_mi <- magick::image_read(corn_r)
	corn_nr <- as.raster(corn, native = TRUE)

	expect_equal(
		bm_padding_lengths(corn_l[[1L]])[["left"]],
		bm_padding_lengths(bm_rotate(corn_l, 90L)[[1L]])[["top"]]
	)

	verify_output(
		"txt/bm_rotate.txt",
		{
			print(bm_rotate(corn, 0L))
			print(bm_rotate(corn, 90L))
			print(bm_rotate(corn, 180L))
			print(bm_rotate(corn, 270L))
			bm_print(bm_rotate(corn_r, 0L))
			bm_print(bm_rotate(corn_r, 90L))
			bm_print(bm_rotate(corn_r, 180L))
			bm_print(bm_rotate(corn_r, 270L))
			bm_print(bm_rotate(corn_mi, 90L))
			bm_print(bm_rotate(corn_mi, 90L, clockwise = FALSE))
			bm_print(bm_rotate(corn_nr, 90L))
		},
		crayon = FALSE,
		unicode = TRUE
	)

	verify_output(
		"txt/bm_rotate_in_place.txt",
		{
			print(bm_rotate(corn, 90L, in_place = TRUE))
			bm_print(bm_rotate(corn_r, 90L, in_place = TRUE))
			bm_print(bm_rotate(corn_mi, 90L, in_place = TRUE))
			bm_print(bm_rotate(corn_nr, 90L, in_place = TRUE))
		},
		crayon = FALSE,
		unicode = TRUE
	)
})

test_that("bm_rotate()", {
	skip_if_not_installed("withr")
	withr::local_options(cli.unicode = TRUE)
	withr::local_options(bm_options(default = TRUE))
	verify_output("txt/capital_r_rotated90.txt", print(bm_rotate(capital_r, 90), px = px_ascii))
	verify_output(
		"txt/capital_r_rotated180.txt",
		print(bm_rotate(capital_r, 180, in_place = TRUE), px = px_ascii)
	)
	verify_output("txt/capital_r_rotated270.txt", print(bm_rotate(capital_r, 270), px = px_ascii))
	verify_output(
		"txt/capital_r_rotatedm90.txt",
		print(bm_rotate(capital_r, 90, clockwise = FALSE), px = px_ascii)
	)
	expect_snapshot(
		error = TRUE,
		bm_rotate(capital_r, 90L, in_place = TRUE)
	)
})
