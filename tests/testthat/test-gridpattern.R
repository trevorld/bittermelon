test_that("`gridpattern` import works", {
    skip_on_ci()
    skip_on_cran()
    skip_if_not_installed("gridpattern")
    skip_if_not_installed("withr")
    skip_if_not(cli::is_utf8_output())
    withr::local_options(bm_options(default = TRUE))

    verify_output("txt/gridpattern.txt", {
        w <- gridpattern::pattern_weave("twill_herringbone", nrow=14L, ncol = 32L)
        pm <- as_bm_pixmap(w, col = c("cyan", "orange"))
        print(pm, compress = "vertical")

        s <- gridpattern::pattern_square(subtype=8L, nrow=8L, ncol = 32L)
        pm <- as_bm_pixmap(s, col = grDevices::rainbow(8L))
        print(pm, compress = "vertical")
    }, unicode = TRUE, crayon = TRUE)
})
