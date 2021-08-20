test_that("read_monobit() and write_monobit()", {
    skip_if_not(findpython::can_find_python_cmd(minimum_version = "3.6"))
    skip_on_cran()

    plus_cp <- name2ucp("PLUS SIGN") # code point U+002B

    hex_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    hex <- read_hex(hex_file)

    hex_file_uncompressed <- tempfile(fileext = ".hex")
    writeLines(readLines(hex_file), hex_file_uncompressed)
    hex2 <- read_monobit(hex_file_uncompressed)
    unlink(hex_file_uncompressed)

    expect_true(is_bm_font(hex2))
    expect_equal(hex[[plus_cp]], hex2[[plus_cp]])

    f <- tempfile(fileext = ".hex")
    write_monobit(hex2, f)
    hex3 <- read_monobit(f)
    unlink(f)

    expect_equal(hex[[plus_cp]], hex3[[plus_cp]])
})
