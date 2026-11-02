# `as_bm_pixmap.grob()` gives informative errors

    Code
      as_bm_pixmap(grid::circleGrob(), height = 1L)
    Condition
      Error in `as_bm_pixmap.grob()`:
      ! height > 1L is not TRUE

# `as_bm_pixmap.bitmap()`

    Code
      as_bm_pixmap(bad)
    Condition
      Error:
      ! Don't know how to handle `bitmap` channel layout 'hsl'

---

    Code
      as_bm_pixmap(bad)
    Condition
      Error:
      ! `bitmap` object is missing its channel layout e.g. `c("bitmap", "rgba")`

