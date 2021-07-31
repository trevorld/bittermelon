test_that("read_yaff() and write_yaff()", {
    hex_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    hex <- read_hex(hex_file)

    yaff_file <- system.file("fonts/spleen/spleen-8x16.yaff.gz", package = "bittermelon")
    yaff <- read_yaff(yaff_file)
    expect_true(is_bm_font(yaff))

    expect_true(any(grepl(" * Spleen is released under the BSD 2-Clause",
                          attr(yaff, "comments"))))
    expect_equal(attr(yaff, "properties")$name,
                 "Spleen Medium 16")
    expect_equal(attr(yaff, "properties")[["source-name"]],
                 "spleen-8x16.bdf")

    plus_cp <- name2ucp("PLUS SIGN") # code point U+002B

    expect_equal(hex[[plus_cp]], yaff[[plus_cp]])

    expect_equal(length(read_yaff(textConnection(""))), 0L)

    f <- tempfile(fileext = ".yaff.gz")
    write_yaff(yaff, gzfile(f))
    yaff2 <- read_yaff(f)
    unlink(f)

    expect_equal(yaff[[plus_cp]], yaff2[[plus_cp]])
})
