test_that("ucp_range()", {
    x <- block2ucp("Basic Latin")
    expect_equal(min(x), "U+0020")

    x <- block2ucp("Basic Latin", omit_unnamed = FALSE)
    expect_equal(min(x), "U+0000")

    x <- range2ucp("U+0020..U+003F")
    expect_length(x, 32L)
    expect_equal(min(x), "U+0020")
    expect_equal(max(x), "U+003F")

    x <- range2ucp("U+0020..U+003F", omit_unnamed = FALSE)
    expect_length(x, 32L)

    x <- ucp_sort(x)
    expect_equal(x[1], "U+0020")
    expect_equal(x[length(x)], "U+003F")
})
