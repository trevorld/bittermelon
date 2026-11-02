# `grob` methods restore the graphics device

    Code
      as_bm_bitmap(grid::circleGrob())
    Condition
      Error in `grid.draw()`:
      ! boom

# `as_bm_bitmap.grob()` gives informative errors

    Code
      as_bm_bitmap(grid::circleGrob(), width = 1L)
    Condition
      Error in `as_bm_bitmap.grob()`:
      ! width > 1L is not TRUE

---

    Code
      as_bm_bitmap(grid::circleGrob(), png_device = function(...) NULL)
    Condition
      Error:
      ! `png_device()` did not open a new graphics device.

