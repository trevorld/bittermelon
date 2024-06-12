test_that("`print.bm_pixmap()` works as expected", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    expect_equal(format(as_bm_pixmap(matrix("transparent", 0L, 0L))),
                 character(0L))
    expect_equal(format(as_bm_pixmap(matrix("transparent", 1L, 0L))),
                 character(0L))
    expect_equal(format(as_bm_pixmap(matrix("transparent", 0L, 1L))),
                 character(0L))

    skip_if_not(cli::is_utf8_output())
    skip_if_not_installed("mazing")
    test_pixmap <- function(bg = "transparent",
                            ul = bg, ur = bg, bl = bg, br = bg) {
        as_bm_pixmap(matrix(c(bl, br, ul, ur), byrow = TRUE, ncol = 2L))
    }
    verify_output("txt/print_bm_pixmap.txt", {
        # 1x1
        p1 <- as_bm_pixmap(matrix("transparent", 1L, 1L))
        print(p1, compress = "n")
        print(p1, compress = "n", bg = "magenta")
        print(p1, compress = "v")
        print(p1, compress = "h")
        print(p1, compress = "b")

        p1 <- as_bm_pixmap(matrix("red", 1L, 1L))
        print(p1, compress = "n")
        print(p1, compress = "v")
        print(p1, compress = "h", downscale = TRUE)
        print(p1, compress = "b")

        # 2x2, one color
        p2 <- test_pixmap("transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        # 2x2, two colors, 1/3 split
        p2 <- test_pixmap("transparent", ul = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("transparent", ur = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("transparent", bl = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("transparent", br = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red", ul = "transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red", ur = "transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red", bl = "transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red", br = "transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("blue", ul = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("blue", ur = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("blue", bl = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("blue", br = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        # 2x2, two colors, 2/2 split, vertical
        p2 <- test_pixmap("transparent", ul = "red", bl = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red", ul = "transparent", bl = "transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("blue", ur = "red", br = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        # 2x2, two colors, 2/2 split, horizontal
        p2 <- test_pixmap("transparent", ul = "red", ur = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red", ul = "transparent", ur = "transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("blue", ul = "red", ur = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        # 2x2, two colors, 2/2 split, diagonal
        p2 <- test_pixmap("transparent", ul = "red", br = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("red", ul = "transparent", br = "transparent")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        p2 <- test_pixmap("blue", ul = "red", br = "red")
        print(p2, compress = "n")
        print(p2, compress = "v")
        print(p2, compress = "h")
        print(p2, compress = "b")

        withr::local_seed(42L)
        pmm <- as_bm_pixmap(mazing::maze(14, 14), col = c("red", "white"))
        print(pmm, compress = "v")

    }, crayon = TRUE, unicode = TRUE)


})
