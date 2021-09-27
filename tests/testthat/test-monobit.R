test_that("read_monobit() and write_monobit()", {
    skip_if_not(findpython::can_find_python_cmd(minimum_version = "3.6"))
    skip_on_cran()

    plus_cp <- name2ucp("PLUS SIGN") # code point U+002B

    hex_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    hexo <- read_hex(hex_file)
    hexr <- read_monobit(hex_file, quietly = TRUE)

    expect_true(is_bm_font(hexr))
    expect_equal(hexo[[plus_cp]], hexr[[plus_cp]])

    f <- tempfile(fileext = ".hex")
    write_monobit(hexr, f, quietly = TRUE)
    hexw <- read_monobit(f, quietly = TRUE)
    unlink(f)

    expect_true(is_bm_font(hexw))
    expect_equal(hexr[[plus_cp]], hexw[[plus_cp]])
})
