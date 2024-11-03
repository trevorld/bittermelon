test_that("`lofifonts` import works", {
    skip_if_not_installed("lofifonts", "0.1.1")

    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    verify_output("txt/lofifonts.txt", {
        mat <- lofifonts::bitmap_text_matrix("Hello", "unifont")
        bm <- as_bm_bitmap(mat)
        print(bm, compress = "n")
        pm <- as_bm_pixmap(mat, col = c("white", "red"))
        print(pm, compress = "n")
    }, unicode = TRUE, crayon = TRUE)
})
