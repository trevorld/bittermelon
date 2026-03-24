# `bm_shift()` overflow

    Code
      bm_shift(bm, top = 6L, overflow = "error")
    Condition
      Error in `bm_shift_bitmap()`:
      ! Shifting top by 6 would clip non-padding content.
      i Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around.
    Code
      bm_shift(bm, right = 6L, overflow = "error")
    Condition
      Error in `bm_shift_bitmap()`:
      ! Shifting right by 6 would clip non-padding content.
      i Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around.
    Code
      bm_shift(bm, bottom = 6L, overflow = "error")
    Condition
      Error in `bm_shift_bitmap()`:
      ! Shifting bottom by 6 would clip non-padding content.
      i Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around.
    Code
      bm_shift(bm, left = 6L, overflow = "error")
    Condition
      Error in `bm_shift_bitmap()`:
      ! Shifting left by 6 would clip non-padding content.
      i Use `overflow = 'clip'` to silently clip or `overflow = 'wrap'` to wrap around.

