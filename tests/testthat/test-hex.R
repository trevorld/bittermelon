test_that("read_hex() and write_hex()", {
    font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
    font <- read_hex(font_file)
    expect_true(is_bm_font(font))
})
