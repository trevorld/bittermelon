test_that("`rsvg::rsvg()` import works", {
	skip_if_not_installed("rsvg")
	skip_if_not_installed("farver")
	skip_if_not(capabilities("cairo"))
	filename <- tempfile(fileext = ".svg")
	on.exit(unlink(filename))
	corn <- farming_crops_16x16()$corn$portrait
	bm <- as_bm_bitmap(corn)
	grDevices::svg(filename, width = 16 / 72, height = 16 / 72, bg = "transparent")
	grid::grid.newpage()
	grid::grid.raster(
		as.raster(corn),
		interpolate = FALSE,
		width = grid::unit(1, "npc"),
		height = grid::unit(1, "npc")
	)
	invisible(grDevices::dev.off())

	array_num <- rsvg::rsvg(filename)
	expect_equal(as_bm_pixmap(array_num), corn)
	expect_equal(as_bm_bitmap(array_num), bm)

	array_raw <- rsvg::rsvg_raw(filename)
	expect_type(array_raw, "raw")
	expect_equal(as_bm_pixmap(array_raw), corn)
	expect_equal(as_bm_bitmap(array_raw), bm)

	nr <- rsvg::rsvg_nativeraster(filename)
	expect_equal(as_bm_pixmap(nr), corn)
	expect_equal(as_bm_bitmap(nr), bm)
})
