test_that("`pdftools::pdf_render_page()` import works", {
    skip_if_not_installed("pdftools")

    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    filename <- tempfile(fileext = ".pdf")
    on.exit(unlink(filename))
    pdf(filename, bg = "blue")
    grid::grid.newpage()
    grid::grid.text("")
    invisible(dev.off())

    array_num <- pdftools::pdf_render_page(filename, 1L, numeric = TRUE)
    pm <- as_bm_pixmap(array_num)
    expect_equal(pm[1L, 1L], col2hex("blue"))
    bm <- as_bm_bitmap(array_num)
    expect_equal(bm[1L, 1L], 1L)

    skip("`pdf_render_page(numeric = FALSE)` randomly fails")
    array_raw <- pdftools::pdf_render_page(filename, 1L, numeric = FALSE)
    pm <- as_bm_pixmap(array_raw)
    expect_equal(pm[1L, 1L], col2hex("blue"))
    bm <- as_bm_bitmap(array_raw)
    expect_equal(bm[1L, 1L], 1L)
})
