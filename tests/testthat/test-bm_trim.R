font_file <- system.file("fonts/spleen/spleen-8x16.hex.gz", package = "bittermelon")
font <- read_hex(font_file)
capital_r <- font[[str2ucp("R")]]

test_that("bm_trim()", {
    capital_r_trimmed <- bm_trim(capital_r, c(1, 1, 3, 0))
    expect_equal(nrow(capital_r_trimmed), 12L)
    expect_equal(ncol(capital_r_trimmed), 7L)
    verify_output("txt/capital_r_trimmed.txt",
                  print(capital_r_trimmed, px = c("-", "#")))

    verify_output("txt/capital_r_trimmed_top.txt",
                  print(bm_trim(capital_r, height = 14, vjust = "top"),
                        px = c("-", "#")))
    verify_output("txt/capital_r_trimmed_bottom.txt",
                  print(bm_trim(capital_r, height = 14, vjust = "bottom"),
                        px = c("-", "#")))
    verify_output("txt/capital_r_trimmed_left.txt",
                  print(bm_trim(capital_r, width = 6, hjust = "left"),
                        px = c("-", "#")))
    verify_output("txt/capital_r_trimmed_right.txt",
                  print(bm_trim(capital_r, width = 6, hjust = "right"),
                        px = c("-", "#")))

    capital_r_resized <- bm_resize(capital_r, width = 10, height = 14, vjust = "top")
    verify_output("txt/capital_r_resized.txt",
                  print(capital_r_resized, px = c("-", "#")))
    expect_equal(ncol(capital_r_resized), 10L)
    expect_equal(nrow(capital_r_resized), 14L)

    capital_r_resized2 <- bm_resize(capital_r, width = 7, height = 18, hjust = "left")
    verify_output("txt/capital_r_resized2.txt",
                  print(capital_r_resized2, px = c("-", "#")))
    expect_equal(ncol(capital_r_resized2), 7L)
    expect_equal(nrow(capital_r_resized2), 18L)
})

test_that("bm_shift()", {
    capital_r_shifted <- bm_shift(capital_r, bottom = 2L)
    verify_output("txt/capital_r_shifted.txt",
                  print(capital_r_shifted, px = c("-", "#")))
    expect_equal(nrow(capital_r_shifted), 16L)
    expect_equal(ncol(capital_r_shifted), 8L)
})

test_that("bm_pad()", {
    capital_r_padded <- bm_pad(capital_r, sides = 2L)
    verify_output("txt/capital_r_padded.txt",
                  print(capital_r_padded, px = c(".", "#")))
    expect_equal(nrow(capital_r_padded), 14L)
    expect_equal(ncol(capital_r_padded), 11L)
})

test_that("bm_shadow", {
    verify_output("txt/capital_r_shadow.txt",
                  print(bm_shadow(capital_r), px = px_ascii))
    verify_output("txt/capital_r_bold.txt",
                  print(bm_bold(capital_r), px = px_ascii))
    verify_output("txt/capital_r_glow.txt",
                  print(bm_glow(capital_r), px = px_ascii))
    verify_output("txt/capital_r_glow_corner.txt",
                  print(bm_glow(capital_r, corner = TRUE), px = px_ascii))
})


test_that("bm_distort()", {
    skip_if_not(capabilities("png"))
    verify_output("txt/capital_r_distorted.txt",
                  print(bm_distort(capital_r, width = 9L, height = 21L), px = px_ascii))
})

test_that("bm_rotate()", {
    verify_output("txt/capital_r_rotated90.txt",
                  print(bm_rotate(capital_r, 90), px = px_ascii))
    verify_output("txt/capital_r_rotated180.txt",
                  print(bm_rotate(capital_r, 180), px = px_ascii))
    verify_output("txt/capital_r_rotated270.txt",
                  print(bm_rotate(capital_r, 270), px = px_ascii))
    verify_output("txt/capital_r_rotatedm90.txt",
                  print(bm_rotate(capital_r, 90, clockwise = FALSE), px = px_ascii))
})
