test_that("`bm_options()` works as expected", {
    skip_if_not_installed("withr")
    withr::local_options(bm_options(default = TRUE))
    expect_equal(bm_options(bittermelon.fg = FALSE),
                 bm_options(default = TRUE))
})
