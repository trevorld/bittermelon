test_that("read_hex() and write_hex()", {
    font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    font <- read_hex(font_file)
    expect_length(font, 837L)
    expect_true(is_bm_font(font))

    font <- read_hex(font_file, ucp = block2ucp("Basic Latin"))
    expect_length(font, 95L)

    expect_true(any(grepl(" * Spleen is released under the BSD 2-Clause",
                          attr(font, "comments"))))

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

    f <- tempfile(fileext = ".hex")
    write_hex(f32, f)
    f32_2 <- read_hex(f)
    unlink(f)

    expect_equal(f32[[c32]], f32_2[[c32]])

    expect_equal(length(read_hex(textConnection(""))), 0L)

    skip_if(!cli::is_utf8_output())
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    verify_output("txt/glyph32_with_9.txt",
                  print(f32[[c32]]),
                  unicode = TRUE)
})
