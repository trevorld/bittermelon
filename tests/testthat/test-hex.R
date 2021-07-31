test_that("read_hex() and write_hex()", {
    font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    font <- read_hex(font_file)
    expect_true(is_bm_font(font))

    f <- tempfile(fileext = ".hex.gz")
    write_hex(font, gzfile(f))
    font2 <- read_hex(f)
    unlink(f)

    plus_sign_code_point <- name2ucp("PLUS SIGN") # code point U+002B
    expect_equal(font[[plus_sign_code_point]], font2[[plus_sign_code_point]])

    # 16x16 hex glyph with a 9 in it
    h32 <- "06DC:0000012809F80900060000000000000000000000000000000000000000000000"
    c32 <- "U+06DC"
    f32 <- read_hex(textConnection(h32))
    verify_output("txt/glyph32_with_9.txt",
                  print(f32[[c32]]))

    f <- tempfile(fileext = ".hex")
    write_hex(f32, f)
    f32_2 <- read_hex(f)
    unlink(f)

    expect_equal(f32[[c32]], f32_2[[c32]])

})